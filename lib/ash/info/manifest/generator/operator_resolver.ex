# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.OperatorResolver do
  @moduledoc """
  Resolves which predicate operators and functions in the filter capabilities
  catalog apply to a given field type, pre-resolving the right-hand-side type
  for each match.

  Treats the field as the *first argument* of each operator/function signature.
  `:any` and `:same` arg-kind sentinels always match. Concrete builtins match
  when the type's `kind` agrees; concrete modules match when the type's
  `module` agrees. `c:Ash.Type.operator_overloads/0` is consulted for the
  type's module — any operator listed there is considered applicable.

  ## NewType subtype matching

  When the field's type module is an `Ash.Type.NewType`, applicability is
  evaluated against both the NewType and its `subtype_of/0` base type. This
  mirrors how ash_graphql derives filter inputs for NewType fields — operators
  with concrete signatures (e.g. `:contains` requires `:string`) match the
  underlying base.

  ## Ordered comparison trimming

  For `:array`- and `:boolean`-kind field types, ordered comparison
  operators (`:<`, `:>`, `:<=`, `:>=`) are dropped from the result. They
  survive the structural signature match (their sigs are `[:same, :any]`,
  which accepts anything) but the orderings aren't useful: no common Ash
  data layer offers a meaningful whole-array compare (Postgres'
  lexicographic compare is rarely what users want, Ets has no notion at
  all), and booleans have no meaningful ordering for filter UX. Equality,
  membership, and `is_nil` are preserved.

  ## RHS resolution

  For each applicable entry, the matching signature's *second* arg drives the
  `rhs` value on the returned `%ApplicableOperator{}`/`%ApplicableFunction{}`.
  Single-arg signatures (e.g. `Eq`'s `[:any]` and `[:same]` shorthand) yield
  `rhs: :same`. Multi-arg signatures yield:

    * second arg `:same`        → `:same`
    * second arg `:any`         → `:any`
    * second arg concrete       → `{:concrete, type_ref}`
    * second arg `{:array, t}`  → `{:array, rhs(t)}`

  Operators that apply only via `c:Ash.Type.operator_overloads/0` (no matching
  signature found structurally) default to `rhs: :same`.
  """

  alias Ash.Info.Manifest.{
    ApplicableFunction,
    ApplicableOperator,
    ArgumentSignature,
    FilterCapabilities,
    Type
  }

  alias Ash.Info.Manifest.Generator.TypeResolver

  @ordered_comparison_operators MapSet.new([:<, :>, :<=, :>=])

  @doc """
  Resolve applicable operators and functions for a field type.

  Returns `{[ApplicableOperator.t()], [ApplicableFunction.t()]}` — each entry
  pairs an operator/function name with its pre-resolved right-hand-side type.

  When `resource_data_layer` is given, data-layer-tagged functions
  (`%Function{data_layer_module: dl}` with `dl != nil`) are filtered to only
  those whose `data_layer_module` matches. Untagged (builtin) functions are
  always considered. Pass `nil` to hide all data-layer-tagged functions —
  use the default `resolve/2` when there's no data-layer context.
  """
  @spec resolve(Type.t(), FilterCapabilities.t(), module() | nil) ::
          {[ApplicableOperator.t()], [ApplicableFunction.t()]}
  def resolve(%Type{} = field_type, %FilterCapabilities{} = caps, resource_data_layer \\ nil) do
    predicate_ops = MapSet.new(caps.predicate_operators)
    predicate_fns = MapSet.new(caps.predicate_functions)
    overloaded = overloaded_operator_names(field_type)
    candidates = candidate_types(field_type)
    op_blacklist = blacklisted_operators(field_type)

    operators =
      caps.operators
      |> Enum.filter(fn op ->
        op.name in predicate_ops and op.name not in op_blacklist
      end)
      |> Enum.flat_map(fn op ->
        case match_with_rhs(op, candidates) do
          {:ok, rhs} ->
            [%ApplicableOperator{name: op.name, rhs: rhs}]

          :error ->
            if op.name in overloaded do
              [%ApplicableOperator{name: op.name, rhs: :same}]
            else
              []
            end
        end
      end)
      |> Enum.uniq_by(& &1.name)

    functions =
      caps.functions
      |> Enum.filter(fn fun ->
        fun.name in predicate_fns and data_layer_visible?(fun, resource_data_layer)
      end)
      |> Enum.flat_map(fn fun ->
        case match_with_rhs(fun, candidates) do
          {:ok, rhs} -> [%ApplicableFunction{name: fun.name, rhs: rhs}]
          :error -> []
        end
      end)
      |> Enum.uniq_by(& &1.name)

    {operators, functions}
  end

  @doc """
  Resolve applicable custom expressions for a field type.

  Same shape as the operator/function resolution but against
  `caps.custom_expressions` filtered by `predicate_custom_expressions`.
  Returns a list of `%ApplicableCustomExpression{}`.
  """
  @spec resolve_custom_expressions(Type.t(), FilterCapabilities.t()) ::
          [Ash.Info.Manifest.ApplicableCustomExpression.t()]
  def resolve_custom_expressions(%Type{} = field_type, %FilterCapabilities{} = caps) do
    predicate_ces = MapSet.new(caps.predicate_custom_expressions)
    candidates = candidate_types(field_type)

    caps.custom_expressions
    |> Enum.filter(&(&1.name in predicate_ces))
    |> Enum.flat_map(fn ce ->
      case match_with_rhs(ce, candidates) do
        {:ok, rhs} ->
          [%Ash.Info.Manifest.ApplicableCustomExpression{name: ce.name, rhs: rhs}]

        :error ->
          []
      end
    end)
    |> Enum.uniq_by(& &1.name)
  end

  defp data_layer_visible?(%{data_layer_module: nil}, _resource_data_layer), do: true
  defp data_layer_visible?(%{data_layer_module: dl}, dl), do: true
  defp data_layer_visible?(_, _), do: false

  # Try each signature against each candidate. First match wins; returns the
  # signature so we can derive the RHS from its second arg.
  defp match_with_rhs(%{signatures: :var_args}, _candidates), do: :error
  defp match_with_rhs(%{signatures: []}, _candidates), do: :error

  defp match_with_rhs(%{signatures: signatures}, candidates) when is_list(signatures) do
    Enum.find_value(signatures, :error, fn sig ->
      if Enum.any?(candidates, &first_arg_matches?(sig, &1)) do
        {:ok, rhs_from_signature(sig)}
      end
    end)
  end

  defp match_with_rhs(_, _), do: :error

  defp rhs_from_signature(%ArgumentSignature{args: [_only]}), do: :same

  defp rhs_from_signature(%ArgumentSignature{args: [_first, second | _]}) do
    arg_to_rhs(second)
  end

  defp rhs_from_signature(_), do: :any

  defp arg_to_rhs(%{kind: :same}), do: :same
  defp arg_to_rhs(%{kind: :any}), do: :any

  defp arg_to_rhs(%{kind: :concrete, type_ref: module}) when not is_nil(module),
    do: {:concrete, module}

  defp arg_to_rhs(%{kind: :array, of: inner}), do: {:array, arg_to_rhs(inner)}
  defp arg_to_rhs(_), do: :any

  defp first_arg_matches?(%ArgumentSignature{args: [first | _]}, field_type) do
    arg_matches_type?(first, field_type)
  end

  defp first_arg_matches?(_, _), do: false

  defp arg_matches_type?(%{kind: :any}, _), do: true
  defp arg_matches_type?(%{kind: :same}, _), do: true

  defp arg_matches_type?(%{kind: :array, of: inner_spec}, %Type{
         kind: :array,
         item_type: %Type{} = item_type
       }) do
    arg_matches_type?(inner_spec, item_type)
  end

  defp arg_matches_type?(%{kind: :concrete, type_ref: module}, %Type{module: module})
       when not is_nil(module),
       do: true

  defp arg_matches_type?(_, _), do: false

  defp blacklisted_operators(%Type{kind: :array}), do: @ordered_comparison_operators
  defp blacklisted_operators(%Type{kind: :boolean}), do: @ordered_comparison_operators
  defp blacklisted_operators(_), do: MapSet.new()

  defp candidate_types(%Type{module: module} = field_type)
       when is_atom(module) and not is_nil(module) do
    Code.ensure_loaded(module)

    if Ash.Type.NewType.new_type?(module) do
      base = Ash.Type.NewType.subtype_of(module)
      [field_type, TypeResolver.resolve(base)]
    else
      [field_type]
    end
  rescue
    _ -> [field_type]
  end

  defp candidate_types(field_type), do: [field_type]

  defp overloaded_operator_names(%Type{module: module})
       when is_atom(module) and not is_nil(module) do
    Code.ensure_loaded(module)

    if function_exported?(module, :operator_overloads, 0) do
      module.operator_overloads()
      |> Map.keys()
      |> MapSet.new()
    else
      MapSet.new()
    end
  rescue
    _ -> MapSet.new()
  end

  defp overloaded_operator_names(_), do: MapSet.new()
end
