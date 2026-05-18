# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator do
  @moduledoc """
  Main pipeline for generating an `%Ash.Info.Manifest{}` from an OTP app's Ash domains.

  Pipeline:
  1. Discover domains and resources
  2. Optionally filter to specified actions
  3. Run reachability analysis
  4. Build resource and type structs
  5. Produce `%Ash.Info.Manifest{}`
  """

  alias Ash.Info.Manifest.Generator.{
    ActionBuilder,
    CapabilitiesBuilder,
    Reachability,
    ResourceBuilder,
    TypeResolver
  }

  @doc """
  Generate an API specification.

  ## Options

    * `:otp_app` - The OTP app to scan (required)
    * `:action_entrypoints` - Optional list of action entrypoints. Each entry can be:
      * `{resource_module, action_name}` — simple tuple (config defaults to `%{}`)
      * `%{resource: module, action: atom, config: map}` — with extension-specific config
      When omitted, all actions across all domains are included.
    * `:overrides` - Optional keyword list of overrides:
      * `:always` - Keyword list of items to always include regardless of reachability:
        * `:resources` - List of resource modules. These are added as reachability roots
          (with no action arguments traversed) so their field types and relationships
          are also discovered.
        * `:types` - List of Ash type modules to include as standalone types directly.

  ## Visibility Options

  By default, only items with `public?: true` are included. Set any of these to `true`
  to also include private items:

    * `:include_private_attributes?` - Include private attributes (default: `false`)
    * `:include_private_calculations?` - Include private calculations (default: `false`)
    * `:include_private_aggregates?` - Include private aggregates (default: `false`)
    * `:include_private_relationships?` - Include private relationships (default: `false`)
    * `:include_private_arguments?` - Include private action arguments (default: `false`)
    * `:include_private_actions?` - Include private actions (default: `false`).
      Private actions (`public? false`) are omitted from `entrypoints` even
      when explicitly named in `:action_entrypoints`.
  """
  @build_opt_keys [
    :include_private_attributes?,
    :include_private_calculations?,
    :include_private_aggregates?,
    :include_private_relationships?,
    :include_private_arguments?,
    :include_private_actions?
  ]

  @spec generate(keyword()) :: {:ok, Ash.Info.Manifest.t()} | {:error, term()}
  def generate(opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)
    action_filter = Keyword.get(opts, :action_entrypoints)
    overrides = Keyword.get(opts, :overrides, [])
    build_opts = Keyword.take(opts, @build_opt_keys)

    always_opts = Keyword.get(overrides, :always, [])
    always_resources = Keyword.get(always_opts, :resources, [])
    always_types = Keyword.get(always_opts, :types, [])

    # Discover all domains and their resources
    domains = Ash.Info.domains(otp_app)

    # Normalize entrypoints and build resource → action_names map for reachability
    normalized_entries = normalize_action_filter(action_filter)
    resource_action_map = build_resource_action_map(domains, action_filter)

    # Build reachability entries: when filtering by actions, pass {resource, action_names}
    # so reachability only traverses arguments of included actions
    reachability_entries =
      if action_filter do
        Enum.map(resource_action_map, fn {resource, action_names} ->
          {resource, action_names || []}
        end)
      else
        Map.keys(resource_action_map)
      end

    # Add always-resources as reachability roots with [] action names
    # (include fields/relationships but no action arguments)
    always_resource_entries = Enum.map(always_resources, &{&1, []})
    reachability_entries = reachability_entries ++ always_resource_entries

    # Run reachability analysis
    {reachable_resources, standalone_types} =
      Reachability.find_reachable(reachability_entries, build_opts)

    # Merge always-resources and always-types into reachability results
    reachable_resources = Enum.uniq(reachable_resources ++ always_resources)
    standalone_types = Enum.uniq(standalone_types ++ always_types)

    # Embedded resources are types, not resources — they live in `manifest.types`
    # as `kind: :embedded_resource` entries with their full Resource definition
    # nested in `type.resource`. Domain-owned resources go in `manifest.resources`.
    {embedded_resource_modules, domain_resource_modules} =
      Enum.split_with(reachable_resources, &Ash.Resource.Info.embedded?/1)

    resources_by_data_layer = collect_resources_by_data_layer(domain_resource_modules)

    {filter_capabilities, sort_capabilities} =
      CapabilitiesBuilder.build(resources_by_data_layer: resources_by_data_layer)

    # Build resource specs (no actions — those live in entrypoints)
    resources =
      domain_resource_modules
      |> Enum.sort_by(&Module.split/1)
      |> Enum.map(fn resource ->
        ResourceBuilder.build(resource, build_opts, filter_capabilities)
      end)

    # Build entrypoints — one per normalized entry (not per unique action)
    entrypoints =
      build_entrypoints(
        normalized_entries,
        resource_action_map,
        build_opts
      )

    # Build standalone type specs (full definitions, not references) for
    # named types (enums, NewTypes) plus embedded resources.
    embedded_type_entries =
      embedded_resource_modules
      |> Enum.sort_by(&Module.split/1)
      |> Enum.map(fn mod ->
        resource = ResourceBuilder.build(mod, build_opts, filter_capabilities)

        %Ash.Info.Manifest.Type{
          kind: :embedded_resource,
          name: resource.name,
          module: mod,
          resource_module: mod,
          resource: resource
        }
      end)

    named_type_entries =
      standalone_types
      |> Enum.sort_by(fn module ->
        if is_atom(module) and Code.ensure_loaded?(module) == true do
          Module.split(module)
        else
          [to_string(module)]
        end
      end)
      |> Enum.map(fn type_module ->
        TypeResolver.resolve_definition(type_module)
      end)

    types = named_type_entries ++ embedded_type_entries

    {:ok,
     %Ash.Info.Manifest{
       resources: resources,
       types: types,
       entrypoints: entrypoints,
       filter_capabilities: filter_capabilities,
       sort_capabilities: sort_capabilities
     }}
  end

  # Groups reachable resources by their data layer, dropping resources whose
  # `Ash.DataLayer.data_layer/1` returns `nil`. The resulting map is what
  # `CapabilitiesBuilder` needs to pass a real (non-nil) resource into each
  # data layer's `functions/1` callback.
  defp collect_resources_by_data_layer(resources) do
    resources
    |> Enum.group_by(&Ash.DataLayer.data_layer/1)
    |> Map.delete(nil)
  end

  # Normalizes action_filter entries into {resource, action_name, config} triples.
  # Returns nil when no filter (all actions included).
  defp normalize_action_filter(nil), do: nil

  defp normalize_action_filter(entries) when is_list(entries) do
    Enum.map(entries, fn
      {resource, action_name} ->
        {resource, action_name, %{}}

      %{resource: resource, action: action_name} = entry ->
        {resource, action_name, Map.get(entry, :config, %{})}
    end)
  end

  # Builds resource → unique action_names map for reachability.
  defp build_resource_action_map(domains, nil) do
    for domain <- domains,
        resource <- Ash.Domain.Info.resources(domain),
        reduce: %{} do
      acc -> Map.put(acc, resource, nil)
    end
  end

  defp build_resource_action_map(_domains, action_filter) when is_list(action_filter) do
    Enum.reduce(action_filter, %{}, fn entry, map ->
      {resource, action_name, _config} =
        case entry do
          {r, a} -> {r, a, %{}}
          %{resource: r, action: a} -> {r, a, %{}}
        end

      Map.update(map, resource, [action_name], fn names ->
        if action_name in names, do: names, else: [action_name | names]
      end)
    end)
  end

  # When no filter: one entrypoint per action on each resource
  defp build_entrypoints(nil, resource_action_map, opts) do
    include_private? = Keyword.get(opts, :include_private_actions?, false)

    resource_action_map
    |> Enum.flat_map(fn {resource, _action_names} ->
      resource
      |> Ash.Resource.Info.actions()
      |> Enum.filter(&include_action?(&1, include_private?))
      |> Enum.map(fn action ->
        %Ash.Info.Manifest.Entrypoint{
          resource: resource,
          action: ActionBuilder.build(resource, action, opts)
        }
      end)
    end)
    |> Enum.sort_by(fn e -> {Module.split(e.resource), e.action.name} end)
  end

  # When filtered: one entrypoint per normalized entry (preserves duplicates with different configs)
  defp build_entrypoints(normalized_entries, _resource_action_map, opts) do
    include_private? = Keyword.get(opts, :include_private_actions?, false)

    normalized_entries
    |> Enum.flat_map(fn {resource, action_name, config} ->
      with %{} = action <- Ash.Resource.Info.action(resource, action_name),
           true <- include_action?(action, include_private?) do
        [
          %Ash.Info.Manifest.Entrypoint{
            resource: resource,
            action: ActionBuilder.build(resource, action, opts),
            config: config
          }
        ]
      else
        _ -> []
      end
    end)
    |> Enum.sort_by(fn e -> {Module.split(e.resource), e.action.name} end)
  end

  defp include_action?(action, include_private?) do
    include_private? || Map.get(action, :public?, true)
  end
end
