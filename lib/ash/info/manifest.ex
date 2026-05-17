# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest do
  @moduledoc """
  Generates a language-agnostic API specification from Ash resources and actions.

  Given a list of `{resource, action_name}` tuples or an OTP app, traverses the
  type graph to find all reachable resources and types, producing structured IR
  (Elixir structs) that can also be serialized to JSON.

  ## Operator canonical name vs. alias

  `%Operator{}.name` is the user-facing symbol that appears in
  `Ash.Query.filter` expressions (e.g. `:==`, `:<`, `:in`). The
  `%Operator{}.aliases` list carries internal/legacy module-derived names
  (e.g. `:eq`, `:less_than`) for backward-compatible rendering. New code
  should prefer `name`; alias-substitution is for tools that historically
  rendered the module-derived spelling.
  """

  alias Ash.Info.Manifest.{Entrypoint, Resource}

  @schema_version "1.0.0"

  @type t :: %__MODULE__{
          resources: [Resource.t()],
          types: [Ash.Info.Manifest.Type.t()],
          entrypoints: [Entrypoint.t()],
          filter_capabilities: Ash.Info.Manifest.FilterCapabilities.t() | nil,
          sort_capabilities: Ash.Info.Manifest.SortCapabilities.t() | nil,
          custom: map()
        }

  @type resource_lookup :: %{atom() => Resource.t()}
  @type action_lookup :: %{{atom(), atom()} => Ash.Info.Manifest.Action.t()}
  @type type_lookup :: %{atom() => Ash.Info.Manifest.Type.t()}

  defstruct resources: [],
            types: [],
            entrypoints: [],
            filter_capabilities: nil,
            sort_capabilities: nil,
            custom: %{}

  @doc """
  The schema version of the manifest's JSON serialization format.

  Bumped when the JSON output schema in `Ash.Info.Manifest.JsonSerializer`
  changes in a way downstream tools need to detect.
  """
  @spec schema_version() :: String.t()
  def schema_version, do: @schema_version

  @doc """
  Generate an API specification for the given OTP app.

  ## Options

    * `:otp_app` - The OTP app to scan for Ash domains and resources (required)
    * `:action_entrypoints` - Optional list of `{resource_module, action_name}` tuples
      used as entrypoints for deriving the spec. When omitted, all public actions
      across all domains are included.
  """
  @spec generate(keyword()) :: {:ok, t()} | {:error, term()}
  def generate(opts) do
    Ash.Info.Manifest.Generator.generate(opts)
  end

  @doc """
  Builds a resource lookup map from the spec, keyed by resource module.
  """
  @spec resource_lookup(t()) :: resource_lookup()
  def resource_lookup(%__MODULE__{resources: resources}) do
    Map.new(resources, fn r -> {r.module, r} end)
  end

  @doc """
  Builds an action lookup map from the spec, keyed by `{resource_module, action_name}`.
  """
  @spec action_lookup(t()) :: action_lookup()
  def action_lookup(%__MODULE__{entrypoints: entrypoints}) do
    Map.new(entrypoints, fn e -> {{e.resource, e.action.name}, e.action} end)
  end

  @doc """
  Builds a type lookup map from the spec, keyed by named type module.

  Each entry is the fully-resolved `%Ash.Info.Manifest.Type{}` for a named type
  (Ash.Type.Enum implementations and Ash.Type.NewType subtypes) referenced
  somewhere in the reachable type graph.
  """
  @spec type_lookup(t()) :: type_lookup()
  def type_lookup(%__MODULE__{types: types}) do
    Map.new(types, fn type -> {type.module, type} end)
  end

  @doc """
  Generates a spec and returns the resource lookup map directly.
  """
  @spec generate_resource_lookup(keyword()) :: {:ok, resource_lookup()} | {:error, term()}
  def generate_resource_lookup(opts) do
    {:ok, spec} = generate(opts)
    {:ok, resource_lookup(spec)}
  end

  @doc "Looks up a resource by module. Returns nil if not found."
  @spec get_resource(resource_lookup(), atom()) :: Resource.t() | nil
  def get_resource(resource_lookup, module) when is_map(resource_lookup) do
    Map.get(resource_lookup, module)
  end

  @doc "Looks up a resource by module. Raises if not found."
  @spec get_resource!(resource_lookup(), atom()) :: Resource.t()
  def get_resource!(resource_lookup, module) when is_map(resource_lookup) do
    case Map.get(resource_lookup, module) do
      %Resource{} = r -> r
      nil -> raise "Resource #{inspect(module)} not found in resource lookup"
    end
  end

  @doc "Checks if a resource exists in the lookup."
  @spec has_resource?(resource_lookup(), atom()) :: boolean()
  def has_resource?(resource_lookup, module) when is_map(resource_lookup) do
    Map.has_key?(resource_lookup, module)
  end

  @doc "Gets a field by resource module and field name."
  @spec get_field(resource_lookup(), atom(), atom()) :: Ash.Info.Manifest.Field.t() | nil
  def get_field(resource_lookup, resource_module, field_name) do
    with %Resource{} = r <- Map.get(resource_lookup, resource_module) do
      Resource.get_field(r, field_name)
    end
  end

  @doc "Gets a relationship by resource module and relationship name."
  @spec get_relationship(resource_lookup(), atom(), atom()) ::
          Ash.Info.Manifest.Relationship.t() | nil
  def get_relationship(resource_lookup, resource_module, rel_name) do
    with %Resource{} = r <- Map.get(resource_lookup, resource_module) do
      Resource.get_relationship(r, rel_name)
    end
  end

  @doc """
  Gets a field or relationship by name, checking fields first.

  Returns `%Ash.Info.Manifest.Field{}`, `%Ash.Info.Manifest.Relationship{}`, or nil.
  """
  @spec get_field_or_relationship(resource_lookup(), atom(), atom()) ::
          Ash.Info.Manifest.Field.t() | Ash.Info.Manifest.Relationship.t() | nil
  def get_field_or_relationship(resource_lookup, resource_module, name) do
    case get_field(resource_lookup, resource_module, name) do
      %Ash.Info.Manifest.Field{} = field -> field
      nil -> get_relationship(resource_lookup, resource_module, name)
    end
  end

  @doc "Gets an action by resource module and action name from an action lookup."
  @spec get_action(action_lookup(), atom(), atom()) :: Ash.Info.Manifest.Action.t() | nil
  def get_action(action_lookup, resource_module, action_name) do
    Map.get(action_lookup, {resource_module, action_name})
  end

  @doc "Looks up a named type's full definition by module. Returns nil if not found."
  @spec get_type(type_lookup(), atom()) :: Ash.Info.Manifest.Type.t() | nil
  def get_type(type_lookup, module) when is_map(type_lookup) and is_atom(module) do
    Map.get(type_lookup, module)
  end

  @doc """
  Looks up a named type's full definition by module. Raises if not found.

  A miss indicates a reachability bug: the type was referenced via `:type_ref`
  somewhere in the type graph but was never registered during spec generation.
  Reachability analysis (`Ash.Info.Manifest.Generator.Reachability`) should have
  collected it as a standalone type.
  """
  @spec get_type!(type_lookup(), atom()) :: Ash.Info.Manifest.Type.t()
  def get_type!(type_lookup, module) when is_map(type_lookup) and is_atom(module) do
    case Map.get(type_lookup, module) do
      %Ash.Info.Manifest.Type{} = t ->
        t

      nil ->
        raise """
        Named type #{inspect(module)} not found in type lookup.

        This indicates a reachability bug — the type was referenced via :type_ref
        somewhere in the spec but was not collected as a standalone type during
        generation. Check `Ash.Info.Manifest.Generator.Reachability.find_reachable/2`.
        """
    end
  end

  @doc "Gets an identity by resource module and identity name."
  @spec get_identity(resource_lookup(), atom(), atom()) :: %{keys: [atom()]} | nil
  def get_identity(resource_lookup, resource_module, identity_name) do
    with %Resource{} = r <- Map.get(resource_lookup, resource_module) do
      Resource.get_identity(r, identity_name)
    end
  end

  @doc "Gets the primary key field names by resource module."
  @spec primary_key(resource_lookup(), atom()) :: [atom()]
  def primary_key(resource_lookup, resource_module) do
    case Map.get(resource_lookup, resource_module) do
      %Resource{primary_key: pk} -> pk
      nil -> []
    end
  end

  @doc """
  Builds an operator lookup map from the spec, keyed by operator name atom.

  Returns an empty map when `filter_capabilities` is `nil`.
  """
  @spec operator_lookup(t()) :: %{atom() => Ash.Info.Manifest.Operator.t()}
  def operator_lookup(%__MODULE__{filter_capabilities: nil}), do: %{}

  def operator_lookup(%__MODULE__{filter_capabilities: %{operators: operators}}) do
    Map.new(operators, fn op -> {op.name, op} end)
  end

  @doc """
  Builds a function lookup map from the spec, keyed by function name atom.
  """
  @spec function_lookup(t()) :: %{atom() => Ash.Info.Manifest.Function.t()}
  def function_lookup(%__MODULE__{filter_capabilities: nil}), do: %{}

  def function_lookup(%__MODULE__{filter_capabilities: %{functions: functions}}) do
    Map.new(functions, fn fun -> {fun.name, fun} end)
  end

  @doc """
  Builds a custom-expression lookup map from the spec, keyed by expression name atom.
  """
  @spec custom_expression_lookup(t()) :: %{atom() => Ash.Info.Manifest.CustomExpression.t()}
  def custom_expression_lookup(%__MODULE__{filter_capabilities: nil}), do: %{}

  def custom_expression_lookup(%__MODULE__{
        filter_capabilities: %{custom_expressions: custom_expressions}
      }) do
    Map.new(custom_expressions, fn ce -> {ce.name, ce} end)
  end

  @doc """
  Returns the resolved `%ApplicableOperator{}` list for `{resource_module, field_name}`.

  Returns `[]` when the field exists but is not filterable. Raises if the
  resource or field is unknown.
  """
  @spec applicable_filter_operators(t(), {atom(), atom()}) ::
          [Ash.Info.Manifest.ApplicableOperator.t()]
  def applicable_filter_operators(%__MODULE__{} = manifest, {resource_module, field_name}) do
    case lookup_field!(manifest, resource_module, field_name) do
      %{filter_operators: nil} -> []
      %{filter_operators: list} when is_list(list) -> list
    end
  end

  @doc """
  Returns the resolved `%ApplicableFunction{}` list for `{resource_module, field_name}`.
  See `applicable_filter_operators/2` for behavior.
  """
  @spec applicable_filter_functions(t(), {atom(), atom()}) ::
          [Ash.Info.Manifest.ApplicableFunction.t()]
  def applicable_filter_functions(%__MODULE__{} = manifest, {resource_module, field_name}) do
    case lookup_field!(manifest, resource_module, field_name) do
      %{filter_functions: nil} -> []
      %{filter_functions: list} when is_list(list) -> list
    end
  end

  @doc """
  Returns the resolved `%ApplicableCustomExpression{}` list for `{resource_module, field_name}`.
  See `applicable_filter_operators/2` for behavior.
  """
  @spec applicable_filter_custom_expressions(t(), {atom(), atom()}) ::
          [Ash.Info.Manifest.ApplicableCustomExpression.t()]
  def applicable_filter_custom_expressions(
        %__MODULE__{} = manifest,
        {resource_module, field_name}
      ) do
    case lookup_field!(manifest, resource_module, field_name) do
      %{filter_custom_expressions: nil} -> []
      %{filter_custom_expressions: list} when is_list(list) -> list
    end
  end

  defp lookup_field!(manifest, resource_module, field_name) do
    lookup = resource_lookup(manifest)

    resource =
      case Map.get(lookup, resource_module) do
        %Resource{} = r -> r
        nil -> raise "Resource #{inspect(resource_module)} not found in manifest"
      end

    case Resource.get_field(resource, field_name) do
      %Ash.Info.Manifest.Field{} = field ->
        field

      nil ->
        raise "Field #{inspect(field_name)} not found on resource #{inspect(resource_module)}"
    end
  end
end
