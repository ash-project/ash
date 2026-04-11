# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TypeResolver.ExprAnalyzer do
  @moduledoc false

  alias Ash.Query.Ref

  def resolve_type(resource, dsl_state, expr, known_types \\ %{}) do
    context = %{resource: dsl_state, public?: false}

    case Ash.Filter.hydrate_refs(expr, context) do
      {:ok, hydrated} ->
        case resolve_auto_types(hydrated, resource, known_types) do
          {:ok, resolved} ->
            case Ash.Expr.determine_type(resolved) do
              {:ok, {type, constraints}} ->
                {:ok, type, constraints}

              :error ->
                {:error, unresolvable_error(expr)}
            end

          {:deps, deps} ->
            {:deps, Enum.uniq(deps)}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Walk expression tree to find :auto typed refs and resolve from known_types

  defp resolve_auto_types(hydrated, resource, known_types) do
    resolved =
      Ash.Filter.map(hydrated, fn
        %Ref{attribute: %{type: :auto} = field} = ref ->
          ref_resource = resolve_resource(ref.resource, resource)

          case Map.fetch(known_types, {ref_resource, field.name}) do
            {:ok, {type, constraints}} ->
              {:halt, %{ref | attribute: %{field | type: type, constraints: constraints}}}

            :error ->
              {:halt, ref}
          end

        other ->
          other
      end)

    deps =
      Ash.Filter.flat_map(resolved, fn
        %Ref{attribute: %{type: :auto} = field} = ref ->
          [{resolve_resource(ref.resource, resource), field.name}]

        _ ->
          []
      end)

    case deps do
      [] -> {:ok, resolved}
      deps -> {:deps, deps}
    end
  end

  # If ref.resource is a dsl_state (map), resolve to the actual module
  defp resolve_resource(ref_resource, fallback_resource) when is_map(ref_resource) do
    case Spark.Dsl.Transformer.get_persisted(ref_resource, :module) do
      nil -> fallback_resource
      module -> module
    end
  rescue
    _ -> fallback_resource
  end

  defp resolve_resource(ref_resource, _fallback)
       when is_atom(ref_resource) and not is_nil(ref_resource) do
    ref_resource
  end

  defp resolve_resource(_, fallback_resource), do: fallback_resource

  defp unresolvable_error(expr) do
    "Cannot automatically determine the type of expression: `#{inspect(expr)}`. " <>
      "Either set an explicit type on the calculation (e.g., `calculate :name, :string, expr(...)`) " <>
      "add `type(...)` around the part of the expression that cannot be resolved, " <>
      "or define separately typed calculations for complex sub-expressions."
  end
end
