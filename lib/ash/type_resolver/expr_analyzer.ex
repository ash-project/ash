defmodule Ash.TypeResolver.ExprAnalyzer do
  @moduledoc false

  # Analyzes expressions from `:auto` calculations to determine their type.
  #
  # Works by walking the expression tree, resolving field references to their types,
  # and then using `Ash.Expr.determine_type/1` on the hydrated expression.
  #
  # Returns:
  # - `{:ok, type, constraints}` if the type can be fully determined
  # - `{:deps, [{resource, field_name}]}` if some cross-resource field types are needed first
  # - `{:error, reason}` if the expression type cannot be determined

  alias Ash.Query.{BooleanExpression, Call, Ref}

  @inline_aggregates [:count, :first, :sum, :list, :max, :min, :avg, :exists, :custom_aggregate]

  @doc """
  Resolves the type of an expression for an `:auto` calculation.

  `resource` is the module being compiled.
  `dsl_state` is the current resource's DSL state.
  `expr` is the raw expression from the calculation.
  `known_types` is a map of `{resource, field_name} => {type, constraints}` for
  cross-resource fields that have been resolved by the TypeResolver.
  """
  def resolve_type(resource, dsl_state, expr, known_types \\ %{}) do
    case hydrate_expr(expr, resource, dsl_state, known_types) do
      {:ok, hydrated} ->
        case Ash.Expr.determine_type(hydrated) do
          {:ok, {type, constraints}} ->
            {:ok, type, constraints}

          :error ->
            # Try our own map literal handling
            case resolve_map_type(hydrated, resource, dsl_state, known_types) do
              {:ok, _, _} = result -> result
              _ -> {:error, unresolvable_error(expr)}
            end
        end

      {:deps, deps} ->
        {:deps, Enum.uniq(deps)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Hydrates an expression just enough that `Ash.Expr.determine_type/1` can work.
  # Replaces atom attributes in Refs with field structs that carry type/constraints.
  # Resolves Call nodes to function/operator structs where possible.

  # Ref with atom attribute and no relationship path (local field)
  defp hydrate_expr(
         %Ref{relationship_path: [], attribute: name} = ref,
         resource,
         dsl_state,
         known_types
       )
       when is_atom(name) do
    case lookup_field(dsl_state, name) do
      {:ok, %{type: :auto} = field} ->
        case Map.fetch(known_types, {resource, name}) do
          {:ok, {type, constraints}} ->
            {:ok, %{ref | attribute: %{field | type: type, constraints: constraints}}}

          :error ->
            {:deps, [{resource, name}]}
        end

      {:ok, field} ->
        {:ok, %{ref | attribute: field}}

      :error ->
        {:error, "Unknown field `#{name}` on `#{inspect(resource)}`"}
    end
  end

  # Ref with relationship path (cross-resource field)
  defp hydrate_expr(
         %Ref{relationship_path: path, attribute: name} = ref,
         _resource,
         dsl_state,
         known_types
       )
       when is_atom(name) and path != [] do
    case follow_relationship_path(dsl_state, path) do
      {:ok, dest_resource} ->
        case lookup_field_on_module(dest_resource, name) do
          {:ok, %{type: :auto} = field} ->
            case Map.fetch(known_types, {dest_resource, name}) do
              {:ok, {type, constraints}} ->
                {:ok, %{ref | attribute: %{field | type: type, constraints: constraints}}}

              :error ->
                {:deps, [{dest_resource, name}]}
            end

          {:ok, field} ->
            {:ok, %{ref | attribute: field}}

          :error ->
            {:error, "Unknown field `#{name}` on `#{inspect(dest_resource)}`"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Already hydrated ref (attribute is a struct with type)
  defp hydrate_expr(%Ref{attribute: %{type: _type}} = ref, _resource, _dsl, _known) do
    {:ok, ref}
  end

  # Call: dot access (post.title) -> convert to Ref with relationship path
  defp hydrate_expr(
         %Call{
           name: :.,
           args: [%Ref{attribute: rel_name, relationship_path: path}, field_name]
         },
         resource,
         dsl_state,
         known_types
       )
       when is_atom(rel_name) and is_atom(field_name) do
    hydrate_expr(
      %Ref{relationship_path: path ++ [rel_name], attribute: field_name},
      resource,
      dsl_state,
      known_types
    )
  end

  # Call: operator
  defp hydrate_expr(
         %Call{name: op, args: [left, right], operator?: true},
         resource,
         dsl_state,
         known_types
       ) do
    with {:ok, h_left} <- hydrate_expr(left, resource, dsl_state, known_types),
         {:ok, h_right} <- hydrate_expr(right, resource, dsl_state, known_types) do
      case Ash.Filter.get_operator(op) do
        nil ->
          {:error, "Unknown operator `#{op}`"}

        op_module ->
          case Ash.Query.Operator.new(op_module, h_left, h_right) do
            {:ok, operator} -> {:ok, operator}
            {:error, _} -> {:error, "Cannot construct operator `#{op}`"}
          end
      end
    else
      {:deps, deps} -> {:deps, deps}
      {:error, reason} -> {:error, reason}
    end
  end

  # Call: inline aggregate (count, sum, first, etc.)
  defp hydrate_expr(
         %Call{name: name, args: args} = call,
         resource,
         dsl_state,
         known_types
       )
       when name in @inline_aggregates do
    resolve_inline_aggregate_type(name, args, call, resource, dsl_state, known_types)
  end

  # Call: type() cast
  defp hydrate_expr(
         %Call{name: :type, args: [expr, type | rest]},
         resource,
         dsl_state,
         known_types
       ) do
    with {:ok, h_expr} <- hydrate_expr(expr, resource, dsl_state, known_types) do
      constraints = List.first(rest) || []

      case Ash.Query.Function.Type.new([h_expr, type, constraints]) do
        {:ok, func} -> {:ok, func}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  # Call: known function
  defp hydrate_expr(
         %Call{name: name, args: args, operator?: false} = _call,
         resource,
         dsl_state,
         known_types
       )
       when is_atom(name) do
    case hydrate_list(args, resource, dsl_state, known_types) do
      {:ok, h_args} ->
        # Try to find the function module
        func_module =
          try do
            Ash.Filter.get_function(name, resource, false)
          rescue
            _ -> nil
          end

        if func_module do
          case func_module.new(h_args) do
            {:ok, func} -> {:ok, func}
            {:error, _} -> {:error, "Cannot resolve function `#{name}` for type determination"}
          end
        else
          {:error,
           "Cannot determine the type of function call `#{name}/#{length(args)}`. " <>
             "Use an explicit type on the calculation, or wrap this part in a separately typed calculation."}
        end

      {:deps, deps} ->
        {:deps, deps}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # BooleanExpression -> Boolean
  defp hydrate_expr(%BooleanExpression{} = expr, _resource, _dsl, _known) do
    {:ok, expr}
  end

  # Exists -> Boolean
  defp hydrate_expr(%Ash.Query.Exists{} = expr, _resource, _dsl, _known) do
    {:ok, expr}
  end

  # Not -> Boolean
  defp hydrate_expr(%Ash.Query.Not{} = expr, _resource, _dsl, _known) do
    {:ok, expr}
  end

  # Parent
  defp hydrate_expr(%Ash.Query.Parent{expr: expr} = parent, resource, dsl_state, known_types) do
    case hydrate_expr(expr, resource, dsl_state, known_types) do
      {:ok, hydrated} -> {:ok, %{parent | expr: hydrated}}
      other -> other
    end
  end

  # UpsertConflict - has an :attribute field, determine_type handles it
  defp hydrate_expr(%Ash.Query.UpsertConflict{} = uc, _resource, _dsl, _known) do
    {:ok, uc}
  end

  # Function.Type (already resolved)
  defp hydrate_expr(%Ash.Query.Function.Type{} = func, _resource, _dsl, _known) do
    {:ok, func}
  end

  # Function.GetPath
  defp hydrate_expr(
         %Ash.Query.Function.GetPath{arguments: [left, path]} = func,
         resource,
         dsl_state,
         known_types
       ) do
    case hydrate_expr(left, resource, dsl_state, known_types) do
      {:ok, h_left} -> {:ok, %{func | arguments: [h_left, path]}}
      other -> other
    end
  end

  # Predicate with arguments (resolved function)
  defp hydrate_expr(
         %{__predicate__?: _, arguments: args} = func,
         resource,
         dsl_state,
         known_types
       ) do
    case hydrate_list(args, resource, dsl_state, known_types) do
      {:ok, h_args} -> {:ok, %{func | arguments: h_args}}
      other -> other
    end
  end

  # Predicate with left/right (resolved operator)
  defp hydrate_expr(
         %{__predicate__?: _, left: left, right: right} = op,
         resource,
         dsl_state,
         known_types
       ) do
    with {:ok, h_left} <- hydrate_expr(left, resource, dsl_state, known_types),
         {:ok, h_right} <- hydrate_expr(right, resource, dsl_state, known_types) do
      {:ok, %{op | left: h_left, right: h_right}}
    end
  end

  # Map literal
  defp hydrate_expr(map, resource, dsl_state, known_types)
       when is_map(map) and not is_struct(map) do
    Enum.reduce_while(map, {:ok, %{}}, fn {key, val}, {:ok, acc} ->
      case hydrate_expr(val, resource, dsl_state, known_types) do
        {:ok, hydrated} -> {:cont, {:ok, Map.put(acc, key, hydrated)}}
        {:deps, deps} -> {:halt, {:deps, deps}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  # List
  defp hydrate_expr(list, resource, dsl_state, known_types) when is_list(list) do
    hydrate_list(list, resource, dsl_state, known_types)
  end

  # Tuple (key-value pair from map)
  defp hydrate_expr({key, value}, resource, dsl_state, known_types)
       when is_atom(key) or is_binary(key) do
    case hydrate_expr(value, resource, dsl_state, known_types) do
      {:ok, hydrated} -> {:ok, {key, hydrated}}
      other -> other
    end
  end

  # Literals
  defp hydrate_expr(value, _resource, _dsl, _known)
       when is_binary(value) or is_integer(value) or is_float(value) or is_boolean(value) or
              is_nil(value) do
    {:ok, value}
  end

  defp hydrate_expr(value, _resource, _dsl, _known) when is_atom(value) do
    {:ok, value}
  end

  # Catch-all: can't hydrate this expression form
  defp hydrate_expr(expr, _resource, _dsl, _known) do
    {:error,
     "Cannot determine the type of expression: `#{inspect(expr)}`. " <>
       "Use an explicit type on the calculation, or define typed calculations for sub-expressions."}
  end

  # Helpers

  defp hydrate_list(list, resource, dsl_state, known_types) do
    Enum.reduce_while(list, {:ok, []}, fn item, {:ok, acc} ->
      case hydrate_expr(item, resource, dsl_state, known_types) do
        {:ok, hydrated} -> {:cont, {:ok, acc ++ [hydrated]}}
        {:deps, deps} -> {:halt, {:deps, deps}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp lookup_field(dsl_state, name) do
    cond do
      attr = Ash.Resource.Info.attribute(dsl_state, name) -> {:ok, attr}
      calc = Ash.Resource.Info.calculation(dsl_state, name) -> {:ok, calc}
      agg = Ash.Resource.Info.aggregate(dsl_state, name) -> {:ok, agg}
      true -> :error
    end
  end

  defp lookup_field_on_module(resource, name) do
    cond do
      attr = Ash.Resource.Info.attribute(resource, name) -> {:ok, attr}
      calc = Ash.Resource.Info.calculation(resource, name) -> {:ok, calc}
      agg = Ash.Resource.Info.aggregate(resource, name) -> {:ok, agg}
      true -> :error
    end
  rescue
    _ -> :error
  end

  defp follow_relationship_path(dsl_state, []) do
    {:ok, Spark.Dsl.Transformer.get_persisted(dsl_state, :module)}
  end

  defp follow_relationship_path(dsl_state, [rel_name | rest]) do
    case Ash.Resource.Info.relationship(dsl_state, rel_name) do
      %{destination: dest} ->
        if rest == [] do
          {:ok, dest}
        else
          # For deeper paths, we need the destination module's info
          try do
            follow_relationship_path_on_module(dest, rest)
          rescue
            _ ->
              {:error,
               "Cannot resolve relationship path #{inspect([rel_name | rest])} - destination resource `#{inspect(dest)}` may not be compiled yet"}
          end
        end

      nil ->
        {:error,
         "Unknown relationship `#{rel_name}` in relationship path on `#{inspect(Spark.Dsl.Transformer.get_persisted(dsl_state, :module))}`"}
    end
  end

  defp follow_relationship_path_on_module(resource, []) do
    {:ok, resource}
  end

  defp follow_relationship_path_on_module(resource, [rel_name | rest]) do
    case Ash.Resource.Info.relationship(resource, rel_name) do
      %{destination: dest} ->
        follow_relationship_path_on_module(dest, rest)

      nil ->
        {:error,
         "Unknown relationship `#{rel_name}` on `#{inspect(resource)}`"}
    end
  end

  defp resolve_inline_aggregate_type(kind, args, _call, resource, dsl_state, known_types) do
    # For aggregates that don't need a field type
    case kind do
      :count ->
        {:ok, Ash.Type.Integer, []}

      :exists ->
        {:ok, Ash.Type.Boolean, []}

      :avg ->
        {:ok, :float, []}

      kind when kind in [:first, :sum, :max, :min, :list] ->
        # These need the field type from the relationship target
        resolve_aggregate_field_type(kind, args, resource, dsl_state, known_types)

      _ ->
        {:error,
         "Cannot determine type for aggregate `#{kind}`. Use an explicit type on the calculation."}
    end
  end

  defp resolve_aggregate_field_type(kind, args, _resource, dsl_state, known_types) do
    # args are typically [relationship_ref] or [relationship_ref, field_ref]
    # At DSL time: count(comments) -> args = [%Ref{attribute: :comments}]
    # first(comments, :title) -> args = [%Ref{attribute: :comments}, :title]
    case args do
      [%Ref{attribute: rel_name, relationship_path: path} | rest] when is_atom(rel_name) ->
        full_path = path ++ [rel_name]

        case follow_relationship_path(dsl_state, full_path) do
          {:ok, dest_resource} ->
            field_name =
              case rest do
                [field] when is_atom(field) -> field
                _ -> nil
              end

            if field_name do
              case lookup_field_on_module(dest_resource, field_name) do
                {:ok, %{type: :auto}} ->
                  case Map.fetch(known_types, {dest_resource, field_name}) do
                    {:ok, {type, constraints}} ->
                      apply_aggregate_kind(kind, type, constraints)

                    :error ->
                      {:deps, [{dest_resource, field_name}]}
                  end

                {:ok, %{type: type, constraints: constraints}} ->
                  apply_aggregate_kind(kind, type, constraints || [])

                :error ->
                  {:error,
                   "Unknown field `#{field_name}` on `#{inspect(dest_resource)}` for aggregate"}
              end
            else
              # No field specified - for count, exists, etc. the type is known
              # For first/sum/max/min without field, it uses the primary key or first attribute
              {:error,
               "Cannot determine type for `#{kind}` aggregate without an explicit field. Use an explicit type on the calculation."}
            end

          {:error, reason} ->
            {:error, reason}
        end

      _ ->
        {:error,
         "Cannot determine type for aggregate expression. Use an explicit type on the calculation."}
    end
  end

  defp apply_aggregate_kind(kind, attr_type, attr_constraints) do
    case Ash.Query.Aggregate.kind_to_type(kind, attr_type, attr_constraints) do
      {:ok, type, constraints} -> {:ok, type, constraints}
      {:error, reason} -> {:error, reason}
    end
  end

  # Handles map literal type resolution when determine_type returns :error
  defp resolve_map_type(map, resource, dsl_state, known_types)
       when is_map(map) and not is_struct(map) do
    Enum.reduce_while(map, {:ok, []}, fn {key, val_expr}, {:ok, acc} ->
      case Ash.Expr.determine_type(val_expr) do
        {:ok, {type, constraints}} ->
          {:cont, {:ok, [{key, [type: type, constraints: constraints]} | acc]}}

        :error ->
          # Try recursive resolution for nested expressions
          case resolve_type(resource, dsl_state, val_expr, known_types) do
            {:ok, type, constraints} ->
              {:cont, {:ok, [{key, [type: type, constraints: constraints]} | acc]}}

            other ->
              {:halt, other}
          end
      end
    end)
    |> case do
      {:ok, fields} ->
        {:ok, Ash.Type.Map, [fields: Enum.reverse(fields)]}

      {:deps, deps} ->
        {:deps, deps}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp resolve_map_type(_, _, _, _), do: :error

  defp unresolvable_error(expr) do
    "Cannot automatically determine the type of expression: `#{inspect(expr)}`. " <>
      "Either set an explicit type on the calculation (e.g., `calculate :name, :string, expr(...)`) " <>
      "or define separately typed calculations for complex sub-expressions."
  end
end
