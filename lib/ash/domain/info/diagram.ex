# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Domain.Info.Diagram do
  @moduledoc """
  Generate Mermaid diagrams from a specified domain.

  ## Limitations

  We can't easily model Ash relationships with Mermaid diagrams
  because they are unidirectional and could be asymmetric.
  Mermaid assumes symmetrical, bidirectional relationships.
  If we try to model all unidirectional relationships as separate
  lines in the diagram it gets very hard to read very quickly.
  """

  @indent "    "
  @show_private? false
  @argument_print_limit 4

  @default_opts indent: @indent, show_private?: @show_private?

  defp resource_name(resource, opts) do
    if opts[:name] == :full do
      inspect(resource)
    else
      resource
      |> Ash.Resource.Info.short_name()
      |> to_string()
      |> Macro.camelize()
    end
  end

  defp domain_name(domain, opts) do
    if opts[:name] == :full do
      inspect(domain)
    else
      domain
      |> Ash.Domain.Info.short_name()
      |> to_string()
      |> Macro.camelize()
    end
  end

  defp short_module(module, opts) do
    if opts[:name] == :full do
      inspect(module)
    else
      module
      |> Module.split()
      |> List.last()
    end
  end

  defp normalise_relationships(domain) do
    for resource <- Ash.Domain.Info.resources(domain) do
      for relationship <- Ash.Resource.Info.relationships(resource) do
        [
          relationship.source,
          relationship.destination
        ]
        |> Enum.sort()
        |> List.to_tuple()
      end
    end
    |> Enum.flat_map(& &1)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp aggregate_type(_resource, %{kind: :custom, type: type}, opts) do
    short_type(type, opts)
  end

  defp aggregate_type(resource, aggregate, opts) do
    attribute =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field)
      end

    attribute_type =
      if attribute do
        attribute.type
      end

    attribute_constraints =
      if attribute do
        attribute.constraints
      end

    {:ok, type, _constraints} =
      Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type, attribute_constraints)

    short_type(type, opts)
  end

  # default to one to one to just show connection
  defp rel_type, do: "||--||"

  defp short_type({:array, t}, opts), do: "ArrayOf#{short_type(t, opts)}"
  defp short_type(t, opts), do: short_module(t, opts)

  @doc """
  Generates a Mermaid Entity Relationship Diagram for a given domain or list of domains.

  Shows only public attributes, calculations, aggregates and actions.
  Shows a one-to-one line for relationships as enumerating all unidirectional
  relationships is far too noisy.
  """

  def mermaid_er_diagram(domains, opts \\ @default_opts) do
    """
    erDiagram
    #{domains |> List.wrap() |> Enum.map(&mermaid_er_domain_section(&1, opts))}\
    """
  end

  defp mermaid_er_domain_section(domain, opts) do
    indent = opts[:indent] || @indent
    show_private? = Access.get(opts, :show_private?, @show_private?)

    resources =
      for resource <- Ash.Domain.Info.resources(domain) do
        {attrs, calcs, aggs} =
          if show_private? do
            {
              Ash.Resource.Info.attributes(resource),
              Ash.Resource.Info.calculations(resource),
              Ash.Resource.Info.aggregates(resource)
            }
          else
            {
              Ash.Resource.Info.public_attributes(resource),
              Ash.Resource.Info.public_calculations(resource),
              Ash.Resource.Info.public_aggregates(resource)
            }
          end

        contents =
          [
            join_template(attrs, indent, &"#{short_type(&1.type, [])} #{&1.name}"),
            join_template(calcs, indent, &"#{short_type(&1.type, [])} #{&1.name}"),
            join_template(aggs, indent, &"#{aggregate_type(resource, &1, [])} #{&1.name}")
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        """
        #{indent}#{inspect(resource_name(resource, opts))} {
        #{contents}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, dest} <- normalise_relationships(domain) do
        ~s(#{indent}#{inspect(resource_name(src, opts))} #{rel_type()} #{inspect(resource_name(dest, opts))} : "")
      end
      |> Enum.join("\n")

    """
    #{resources}
    #{relationships}
    """
  end

  defp class_short_type({:array, t}, opts), do: "#{short_module(t, opts)}[]"
  defp class_short_type(t, opts), do: short_module(t, opts)

  defp join_template(list, indent, template_fn) do
    Enum.map_join(list, "\n", fn item -> "#{indent}#{indent}#{template_fn.(item)}" end)
  end

  defp list_arguments_and_attributes(resource, action, opts) do
    arguments = action.arguments

    attributes =
      case action do
        %{type: :action} -> []
        %{type: :read} -> []
        %{type: :destroy, soft?: false} -> []
        %{accept: accept} -> accept |> Enum.map(&Ash.Resource.Info.attribute(resource, &1))
      end

    list_arguments(Enum.uniq_by(arguments ++ attributes, & &1.name), opts)
  end

  defp list_arguments(arguments, opts) when length(arguments) > @argument_print_limit do
    {displayed_args, _rest} = Enum.split(arguments, @argument_print_limit)
    list_arguments(displayed_args, opts) <> ", ..."
  end

  defp list_arguments(arguments, opts) do
    arguments
    |> Enum.map_join(", ", &"#{class_short_type(&1.type, opts)} #{&1.name}")
  end

  @doc """
  Generates a Mermaid Class Diagram for a given domain.

  Shows only public attributes, calculations, aggregates and actions.
  Shows a connecting line for relationships with the type of relationship
  indicated in the attribute list.
  """
  def mermaid_class_diagram(domain, opts \\ @default_opts)

  def mermaid_class_diagram(domains, opts) when is_list(domains) do
    indent = opts[:indent] || @indent
    show_private? = Access.get(opts, :show_private?, @show_private?)

    resources =
      for domain <- domains, resource <- Ash.Domain.Info.resources(domain) do
        actions = Ash.Resource.Info.actions(resource)

        {attrs, calcs, aggs, relationships} =
          if show_private? do
            {
              Ash.Resource.Info.attributes(resource),
              Ash.Resource.Info.calculations(resource),
              Ash.Resource.Info.aggregates(resource),
              Ash.Resource.Info.relationships(resource)
            }
          else
            {Ash.Resource.Info.public_attributes(resource),
             Ash.Resource.Info.public_calculations(resource),
             Ash.Resource.Info.public_aggregates(resource),
             Ash.Resource.Info.public_relationships(resource)}
          end

        contents =
          [
            join_template(attrs, indent, &"#{class_short_type(&1.type, opts)} #{&1.name}"),
            join_template(calcs, indent, &"#{class_short_type(&1.type, opts)} #{&1.name}"),
            join_template(aggs, indent, &"#{aggregate_type(resource, &1, opts)} #{&1.name}"),
            join_template(
              relationships,
              indent,
              &"#{resource_name(&1.destination, opts)}#{if &1.cardinality == :many, do: "[]", else: ""} #{&1.name}"
            ),
            join_template(
              actions,
              indent,
              &"#{&1.name}(#{list_arguments_and_attributes(resource, &1, opts)})"
            )
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        """
        #{indent}class #{resource_name(resource, opts)} {
        #{indent}#{indent}Domain: #{domain_name(domain, opts)}#{source_link(resource, indent, opts)}
        #{contents}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for domain <- domains,
          {src, dest} <- normalise_relationships(domain) do
        ~s(#{indent}#{resource_name(src, opts)} -- #{resource_name(dest, opts)})
      end
      |> Enum.join("\n")

    domains =
      Enum.map_join(domains, "\n", fn domain ->
        ~s(#{indent}#{domain_name(domain, opts)})
      end)

    """
    classDiagram
    #{domains}
    #{resources}
    #{relationships}
    """
  end

  def mermaid_class_diagram(domain, opts) do
    indent = opts[:indent] || @indent
    show_private? = Access.get(opts, :show_private?, @show_private?)

    resources =
      for resource <- Ash.Domain.Info.resources(domain) do
        actions = Ash.Resource.Info.actions(resource)

        {attrs, calcs, aggs, relationships} =
          if show_private? do
            {
              Ash.Resource.Info.attributes(resource),
              Ash.Resource.Info.calculations(resource),
              Ash.Resource.Info.aggregates(resource),
              Ash.Resource.Info.relationships(resource)
            }
          else
            {Ash.Resource.Info.public_attributes(resource),
             Ash.Resource.Info.public_calculations(resource),
             Ash.Resource.Info.public_aggregates(resource),
             Ash.Resource.Info.public_relationships(resource)}
          end

        contents =
          [
            join_template(attrs, indent, &"#{class_short_type(&1.type, opts)} #{&1.name}"),
            join_template(calcs, indent, &"#{class_short_type(&1.type, opts)} #{&1.name}"),
            join_template(aggs, indent, &"#{aggregate_type(resource, &1, opts)} #{&1.name}"),
            join_template(
              relationships,
              indent,
              &"#{resource_name(&1.destination, opts)}#{if &1.cardinality == :many, do: "[]", else: ""} #{&1.name}"
            ),
            join_template(
              actions,
              indent,
              &"#{&1.name}(#{list_arguments_and_attributes(resource, &1, opts)})"
            )
          ]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        """
        #{indent}class #{resource_name(resource, opts)} {
        #{contents}
        #{indent}}
        """
      end
      |> Enum.join()

    relationships =
      for {src, dest} <- normalise_relationships(domain) do
        ~s(#{indent}#{resource_name(src, opts)} -- #{resource_name(dest, opts)})
      end
      |> Enum.join("\n")

    """
    classDiagram
    #{resources}
    #{relationships}
    """
  end

  defp source_link(module, indent, opts) do
    if opts[:include_source_links?] do
      module.module_info(:compile)[:source]
      |> List.to_string()
      |> Path.relative_to_cwd()
      |> then(&"\n#{indent}#{indent}Source: #{&1}\n")
    end
  rescue
    _ ->
      nil
  end
end
