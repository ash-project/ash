defmodule Ash.Filter do
  @moduledoc """
  The representation of a filter in Ash.

  Ash filters are stored as nested `Ash.Filter.Expression{}` and `%Ash.Filter.Not{}` structs,
  terminating in a `%Ash.Filter.Predicate{}` struct. An expression is simply a boolean operator
  and the left and right hand side of that operator.
  """
  alias Ash.Engine.Request
  alias Ash.Filter.Predicate.{Eq, In}
  alias Ash.Filter.{Expression, Not, Predicate}

  @built_in_predicates [
    eq: Eq,
    in: In
  ]

  defstruct [:resource, :api, :expression]

  def parse!(api, resource, statement) do
    case parse(api, resource, statement) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise error
    end
  end

  def parse(api, resource, statement) do
    context = %{
      resource: resource,
      api: api,
      relationship_path: []
    }

    case parse_expression(statement, context) do
      {:ok, expression} ->
        {:ok, %__MODULE__{expression: expression, resource: resource, api: api}}

      {:error, error} ->
        {:error, error}
    end
  end

  def run_other_data_layer_filters(resource, api, filter) do
    reduce(filter, {:ok, filter}, fn
      %Expression{op: :or}, {:ok, filter} ->
        {:halt, {:ok, filter}}

      %Predicate{} = expression, {:ok, filter} ->
        expression
        |> relationship_paths(:ands_only)
        |> filter_paths_that_change_data_layers(resource)
        |> Enum.reduce_while({:halt, {:ok, filter}}, fn path, {:halt, {:ok, filter}} ->
          {for_path, without_path} = split_expression_by_relationship_path(filter, path)

          relationship = Ash.relationship(resource, path)

          query =
            relationship.destination
            |> api.query()
            |> Map.put(:filter, for_path)

          add_other_datalayer_read_results(query, relationship, path, without_path)
        end)

      %Expression{op: :and} = expression, {:ok, filter} ->
        expression
        |> relationship_paths(:ands_only)
        |> filter_paths_that_change_data_layers(resource)
        |> Enum.reduce_while({:halt, {:ok, filter}}, fn path, {:halt, {:ok, filter}} ->
          {for_path, without_path} = split_expression_by_relationship_path(filter, path)

          relationship = Ash.relationship(resource, path)

          query =
            relationship.destination
            |> api.query()
            |> Map.put(:filter, for_path)

          add_other_datalayer_read_results(query, relationship, path, without_path)
        end)

      _, {:ok, filter} ->
        {:ok, filter}
    end)
  end

  defp add_other_datalayer_read_results(query, relationship, path, filter_without_path) do
    case query.api.read(query) do
      {:ok, results} ->
        new_filter =
          case relationship.type do
            :many_to_many ->
              many_to_many_read_results(results, relationship, query, path)

            _ ->
              results
              |> Enum.map(&Map.get(&1, relationship.destination_field))
              |> Enum.reject(&is_nil/1)
              |> record_filters_or_false(relationship)
              |> put_at_path(:lists.droplast(path))
          end

        case add_to_filter(filter_without_path, new_filter) do
          {:ok, filter} -> {:cont, {:halt, {:ok, filter}}}
          {:error, error} -> {:halt, {:return, {:error, error}}}
        end

      {:error, error} ->
        {:halt, {:return, {:error, error}}}
    end
  end

  defp record_filters_or_false(records, relationship) do
    case records do
      [] ->
        false

      [value] ->
        [{relationship.source_field, value}]

      values ->
        [{relationship.source_field, [in: values]}]
    end
  end

  defp many_to_many_read_results(results, relationship, query, path) do
    destination_values =
      results
      |> Enum.map(&Map.get(&1, relationship.destination_field))
      |> Enum.reject(&is_nil/1)

    join_query =
      relationship.through
      |> query.api.query()
      |> Ash.Query.filter([
        {relationship.destination_field_on_join_table, [in: destination_values]}
      ])

    case query.api.read(join_query) do
      {:ok, results} ->
        results
        |> Enum.map(&Map.get(&1, relationship.source_field_on_join_table))
        |> Enum.reject(&is_nil/1)
        |> case do
          [] ->
            false

          [value] ->
            [{relationship.source_field, value}]

          values ->
            [{relationship.source_field, [in: values]}]
        end
        |> put_at_path(:lists.droplast(path))

      {:error, error} ->
        {:error, error}
    end
  end

  defp filter_paths_that_change_data_layers(paths, resource, acc \\ [])
  defp filter_paths_that_change_data_layers([], _resource, acc), do: acc

  defp filter_paths_that_change_data_layers([path | rest], resource, acc) do
    case shortest_path_to_changed_data_layer(resource, path) do
      {:ok, path} ->
        new_rest = Enum.reject(rest, &List.starts_with?(&1, path))
        filter_paths_that_change_data_layers(new_rest, resource, [path | acc])

      :error ->
        filter_paths_that_change_data_layers(rest, resource, acc)
    end
  end

  defp shortest_path_to_changed_data_layer(resource, path, acc \\ [])
  defp shortest_path_to_changed_data_layer(_resource, [], _acc), do: :error

  defp shortest_path_to_changed_data_layer(resource, [relationship | rest], acc) do
    relationship = Ash.relationship(resource, relationship)
    data_layer = Ash.data_layer(relationship.destination)

    if relationship.type == :many_to_many do
      if data_layer == Ash.data_layer(resource) &&
           data_layer == Ash.data_layer(relationship.through) &&
           Ash.data_layer_can?(resource, :join) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    else
      if data_layer == Ash.data_layer(resource) && Ash.data_layer_can?(resource, :join) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    end
  end

  defp put_at_path(value, []), do: value
  defp put_at_path(value, [key | rest]), do: [{key, put_at_path(value, rest)}]

  def relationship_paths(filter_or_expression, kind \\ :all)
  def relationship_paths(nil, _), do: []
  def relationship_paths(%{expression: nil}, _), do: []

  def relationship_paths(%__MODULE__{expression: expression}, kind),
    do: relationship_paths(expression, kind)

  def relationship_paths(expression, kind) do
    expression
    |> do_relationship_paths(kind)
    |> List.wrap()
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.map(fn {path} -> path end)
  end

  def add_to_filter!(base, op \\ :and, addition) do
    case add_to_filter(base, op, addition) do
      {:ok, value} ->
        value

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  def add_to_filter(base, op \\ :and, addition)

  def add_to_filter(
        %__MODULE__{resource: resource, api: api} = base,
        op,
        %__MODULE__{resource: resource, api: api} = addition
      ) do
    {:ok, %{base | expression: Expression.new(op, base.expression, addition.expression)}}
  end

  def add_to_filter(%__MODULE__{api: api} = base, _, %__MODULE__{api: api} = addition) do
    {:error,
     "Cannot add filter for resource #{inspect(addition.resource)} to filter with resource #{
       inspect(base.resource)
     }"}
  end

  def add_to_filter(
        %__MODULE__{resource: resource} = base,
        _,
        %__MODULE__{resource: resource} = addition
      ) do
    {:error,
     "Cannot add filter for api #{inspect(addition.api)} to filter with api #{inspect(base.api)}"}
  end

  def add_to_filter(%__MODULE__{} = base, op, statement) do
    case parse(base.api, base.resource, statement) do
      {:ok, filter} -> add_to_filter(base, op, filter)
      {:error, error} -> {:error, error}
    end
  end

  @doc """
  Returns true if the second argument is a strict subset (always returns the same or less data) of the first
  """
  def strict_subset_of(nil, _), do: true

  def strict_subset_of(_, nil), do: false

  def strict_subset_of(%{resource: resource}, %{resource: other_resource})
      when resource != other_resource,
      do: false

  def strict_subset_of(filter, candidate) do
    Ash.SatSolver.strict_filter_subset(filter, candidate)
  end

  def strict_subset_of?(filter, candidate) do
    strict_subset_of(filter, candidate) == true
  end

  def relationship_filter_request_paths(filter) do
    filter
    |> relationship_paths()
    |> Enum.map(&[:filter, &1])
  end

  def read_requests(nil), do: {:ok, []}

  def read_requests(filter) do
    filter
    |> Ash.Filter.relationship_paths()
    |> Enum.map(fn path ->
      {path, filter_expression_by_relationship_path(filter, path)}
    end)
    |> Enum.reduce_while({:ok, []}, fn {path, scoped_filter}, {:ok, requests} ->
      %{api: api, resource: resource} = scoped_filter

      with %{errors: []} = query <- Ash.Query.new(api, resource),
           %{errors: []} = query <- Ash.Query.filter(query, scoped_filter),
           {:action, action} when not is_nil(action) <-
             {:action, Ash.primary_action(resource, :read)} do
        request =
          Request.new(
            resource: resource,
            api: api,
            query: query,
            path: [:filter, path],
            strict_check_only?: true,
            action: action,
            data: []
          )

        {:cont, {:ok, [request | requests]}}
      else
        {:error, error} -> {:halt, {:error, error}}
        %{errors: errors} -> {:halt, {:error, errors}}
        {:action, nil} -> {:halt, {:error, "Default read action required"}}
      end
    end)
  end

  def map(%__MODULE__{expression: nil} = filter, _) do
    filter
  end

  def map(%__MODULE__{expression: expression} = filter, func) do
    %{filter | expression: do_map(func.(expression), func)}
  end

  def map(expression, func) do
    do_map(func.(expression), func)
  end

  def do_map(expression, func) do
    case expression do
      {:halt, expr} ->
        expr

      %Expression{left: left, right: right} = expr ->
        %{expr | left: do_map(left, func), right: do_map(right, func)}

      %Not{expression: not_expr} = expr ->
        %{expr | expression: do_map(not_expr, func)}

      other ->
        func.(other)
    end
  end

  def reduce(filter, acc \\ nil, func)
  def reduce(%__MODULE__{expression: nil}, acc, _), do: acc

  def reduce(%__MODULE__{expression: expression}, acc, func) do
    case func.(expression, acc) do
      {:halt, acc} ->
        acc

      {:return, value} ->
        value

      acc ->
        case do_reduce(expression, acc, func) do
          {:halt, acc} -> acc
          {:return, value} -> value
          acc -> acc
        end
    end
  end

  def reduce(expression, acc, func) do
    case func.(expression, acc) do
      {:halt, acc} ->
        acc

      {:return, value} ->
        value

      acc ->
        case do_reduce(expression, acc, func) do
          {:halt, acc} -> acc
          {:return, value} -> value
          acc -> acc
        end
    end
  end

  def do_reduce(expression, acc, func) do
    case expression do
      %Expression{} = expression ->
        do_reduce_expression(expression, acc, func)

      %Not{expression: not_expr} ->
        case func.(not_expr, acc) do
          {:halt, acc} ->
            acc

          {:return, value} ->
            {:return, value}

          acc ->
            do_reduce(not_expr, acc, func)
        end

      {:return, value} ->
        {:return, value}

      {:halt, value} ->
        {:halt, value}

      other ->
        func.(other, acc)
    end
  end

  defp do_reduce_expression(%Expression{left: left, right: right}, acc, func) do
    case func.(right, acc) do
      {:halt, acc} ->
        case func.(left, acc) do
          {:return, value} ->
            {:return, value}

          {:halt, acc} ->
            acc

          acc ->
            do_reduce(left, acc, func)
        end

      {:return, value} ->
        {:return, value}

      acc ->
        continue_reduce(left, right, acc, func)
    end
  end

  defp continue_reduce(left, right, acc, func) do
    case func.(left, acc) do
      {:halt, acc} ->
        do_reduce(right, acc, func)

      {:return, value} ->
        {:return, value}

      acc ->
        case do_reduce(left, acc, func) do
          {:halt, acc} ->
            {:halt, acc}

          {:return, acc} ->
            {:return, acc}

          acc ->
            do_reduce(right, acc, func)
        end
    end
  end

  defp split_expression_by_relationship_path(%{expression: expression} = filter, _path)
       when expression in [nil, true, false] do
    {filter, filter}
  end

  defp split_expression_by_relationship_path(filter, path) do
    {for_path, without_path} = do_split_expression_by_relationship_path(filter.expression, path)

    {%__MODULE__{
       api: filter.api,
       resource: Ash.related(filter.resource, path),
       expression: for_path
     },
     %__MODULE__{
       api: filter.api,
       resource: filter.resource,
       expression: without_path
     }}
  end

  defp filter_expression_by_relationship_path(filter, path) do
    %__MODULE__{
      api: filter.api,
      resource: Ash.related(filter.resource, path),
      expression: do_filter_expression_by_relationship_path(filter.expression, path)
    }
  end

  defp do_split_expression_by_relationship_path(
         %Expression{op: op, left: left, right: right},
         path
       ) do
    {new_for_path_left, new_without_path_left} =
      do_split_expression_by_relationship_path(left, path)

    {new_for_path_right, new_without_path_right} =
      do_split_expression_by_relationship_path(right, path)

    {Expression.new(op, new_for_path_left, new_for_path_right),
     Expression.new(op, new_without_path_left, new_without_path_right)}
  end

  defp do_split_expression_by_relationship_path(%Not{expression: expression}, path) do
    {new_for_path, new_without_path} = do_split_expression_by_relationship_path(expression, path)
    {Not.new(new_for_path), Not.new(new_without_path)}
  end

  defp do_split_expression_by_relationship_path(
         %Predicate{relationship_path: predicate_path} = predicate,
         path
       ) do
    if List.starts_with?(predicate_path, path) do
      {%{predicate | relationship_path: Enum.drop(predicate_path, length(path))}, nil}
    else
      {nil, predicate}
    end
  end

  defp do_filter_expression_by_relationship_path(
         %Expression{op: op, left: left, right: right},
         path
       ) do
    new_left = do_filter_expression_by_relationship_path(left, path)
    new_right = do_filter_expression_by_relationship_path(right, path)

    Expression.new(op, new_left, new_right)
  end

  defp do_filter_expression_by_relationship_path(%Not{expression: expression}, path) do
    new_expression = do_filter_expression_by_relationship_path(expression, path)
    Not.new(new_expression)
  end

  defp do_filter_expression_by_relationship_path(
         %Predicate{relationship_path: predicate_path} = predicate,
         path
       ) do
    if List.starts_with?(predicate_path, path) do
      predicate
    else
      nil
    end
  end

  defp do_relationship_paths(%Predicate{relationship_path: []}, _) do
    []
  end

  defp do_relationship_paths(%Predicate{relationship_path: path}, _) do
    {path}
  end

  defp do_relationship_paths(%Expression{op: :or}, :ands_only) do
    []
  end

  defp do_relationship_paths(%Expression{left: left, right: right}, kind) do
    [do_relationship_paths(left, kind), do_relationship_paths(right, kind)]
  end

  defp do_relationship_paths(%Not{expression: expression}, kind) do
    do_relationship_paths(expression, kind)
  end

  defp parse_expression(%__MODULE__{expression: expression}, context),
    do: {:ok, add_to_predicate_path(expression, context)}

  defp parse_expression(statement, context) when is_map(statement) or is_list(statement) do
    Enum.reduce_while(statement, {:ok, nil}, fn expression_part, {:ok, expression} ->
      case add_expression_part(expression_part, context, expression) do
        {:ok, new_expression} ->
          {:cont, {:ok, new_expression}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_expression(statement, context) do
    parse_expression([statement], context)
  end

  defp add_expression_part(boolean, _context, expression) when is_boolean(boolean),
    do: {:ok, Expression.new(:and, expression, boolean)}

  defp add_expression_part(%__MODULE__{expression: adding_expression}, context, expression) do
    {:ok, Expression.new(:and, expression, add_to_predicate_path(adding_expression, context))}
  end

  defp add_expression_part(%resource{} = record, context, expression) do
    if resource == context.resource do
      pkey_filter = record |> Map.take(Ash.primary_key(resource)) |> Map.to_list()
      add_expression_part(pkey_filter, context, expression)
    else
      {:error, "Invalid filter value provided: #{inspect(record)}"}
    end
  end

  defp add_expression_part({:not, nested_statement}, context, expression) do
    case parse_expression(nested_statement, context) do
      {:ok, nested_expression} ->
        {:ok, Expression.new(:and, expression, Not.new(nested_expression))}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({:or, nested_statements}, context, expression) do
    with {:ok, nested_expression} <- parse_and_join(nested_statements, :or, context),
         :ok <- validate_datalayers_support_boolean_filters(nested_expression) do
      {:ok, Expression.new(:and, expression, nested_expression)}
    end
  end

  defp add_expression_part({:and, nested_statements}, context, expression) do
    case parse_and_join(nested_statements, :and, context) do
      {:ok, nested_expression} ->
        {:ok, Expression.new(:and, expression, nested_expression)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({field, nested_statement}, context, expression)
       when is_atom(field) or is_binary(field) do
    cond do
      attr = Ash.attribute(context.resource, field) ->
        case parse_predicates(nested_statement, attr, context) do
          {:ok, nested_statement} ->
            {:ok, Expression.new(:and, expression, nested_statement)}

          {:error, error} ->
            {:error, error}
        end

      rel = Ash.relationship(context.resource, field) ->
        context =
          context
          |> Map.update!(:relationship_path, fn path -> path ++ [rel.name] end)
          |> Map.put(:resource, rel.destination)

        if is_list(nested_statement) || is_map(nested_statement) do
          case parse_expression(nested_statement, context) do
            {:ok, nested_expression} ->
              {:ok, Expression.new(:and, expression, nested_expression)}

            {:error, error} ->
              {:error, error}
          end
        else
          with [field] <- Ash.primary_key(context.resource),
               attribute <- Ash.attribute(context.resource, field),
               {:ok, casted} <- Ash.Type.cast_input(attribute.type, nested_statement) do
            add_expression_part({field, casted}, context, expression)
          else
            _other ->
              {:error, "Invalid filter value provided: #{inspect(nested_statement)}"}
          end
        end

      true ->
        {:error, "No such attribute or relationship #{field} on #{inspect(context.resource)}"}
    end
  end

  defp add_expression_part(value, context, expression) when is_map(value) do
    # Can't call `parse_expression/2` here because it will loop

    value
    |> Map.to_list()
    |> Enum.reduce_while({:ok, nil}, fn {key, value}, {:ok, expression} ->
      case add_expression_part({key, value}, context, expression) do
        {:ok, new_expression} ->
          {:cont, {:ok, new_expression}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, new_expression} -> {:ok, Expression.new(:and, expression, new_expression)}
      {:error, error} -> {:error, error}
    end
  end

  defp add_expression_part(value, _, _) do
    {:error, "Invalid filter value provided: #{inspect(value)}"}
  end

  defp validate_datalayers_support_boolean_filters(%Expression{op: :or, left: left, right: right}) do
    left_predicates =
      left
      |> reduce([], fn
        %Predicate{} = pred, acc ->
          [{pred.relationship_path, pred.resource} | acc]

        _, acc ->
          acc
      end)
      |> Enum.uniq()

    right_predicates =
      right
      |> reduce([], fn
        %Predicate{} = pred, acc ->
          [{pred.relationship_path, pred.resource} | acc]

        _, acc ->
          acc
      end)
      |> Enum.uniq()

    Enum.reduce_while(left_predicates, :ok, fn {path, resource}, :ok ->
      check_predicate_compatibility(resource, path, right_predicates)
    end)
  end

  defp check_predicate_compatibility(resource, path, right_predicates) do
    can_join? = Ash.data_layer_can?(resource, :join)
    can_boolean_filter? = Ash.data_layer_can?(resource, :boolean_filter)

    cond do
      can_join? and can_boolean_filter? ->
        {:cont, :ok}

      can_join? ->
        check_when_cant_boolean_filter(right_predicates, resource, path)

      can_boolean_filter? ->
        check_when_cant_join(right_predicates, resource, path)

      true ->
        check_when_cant_join_or_boolean_filter(right_predicates, resource)
    end
  end

  defp check_when_cant_join_or_boolean_filter(right_predicates, resource) do
    data_layer = Ash.data_layer(resource)

    if Enum.any?(right_predicates, fn {_, right_resource} ->
         Ash.data_layer(right_resource) == data_layer
       end) do
      {:halt,
       {:error,
        "Data layer #{inspect(Ash.data_layer(resource))} does not support joins or boolean filters, which are necessary for this query"}}
    else
      {:cont, :ok}
    end
  end

  defp check_when_cant_join(right_predicates, resource, path) do
    data_layer = Ash.data_layer(resource)

    right_predicates
    |> Enum.filter(fn {_right_path, right_resource} ->
      Ash.data_layer(right_resource) == data_layer
    end)
    |> Enum.all?(fn {right_path, _} ->
      right_path == path
    end)
    |> case do
      true ->
        {:cont, :ok}

      false ->
        {:halt,
         {:error,
          "Data layer #{inspect(Ash.data_layer(resource))} does not support joins, which are necessary for this query"}}
    end
  end

  defp check_when_cant_boolean_filter(right_predicates, resource, path) do
    if Enum.any?(right_predicates, fn {right_path, right_resource} ->
         right_resource == resource && right_path == path
       end) do
      {:halt,
       {:error,
        "Data layer #{inspect(Ash.data_layer(resource))} does not support boolean filters"}}
    else
      {:cont, :ok}
    end
  end

  defp add_to_predicate_path(expression, context) do
    case expression do
      %Not{expression: expression} = not_expr ->
        %{not_expr | expression: add_to_predicate_path(expression, context)}

      %Expression{left: left, right: right} = expression ->
        %{
          expression
          | left: add_to_predicate_path(left, context),
            right: add_to_predicate_path(right, context)
        }

      %Predicate{relationship_path: relationship_path} = pred ->
        %{pred | relationship_path: context.relationship_path ++ relationship_path}
    end
  end

  defp parse_and_join(statements, op, context) do
    Enum.reduce_while(statements, {:ok, nil}, fn statement, {:ok, expression} ->
      case parse_expression(statement, context) do
        {:ok, nested_expression} ->
          {:cont, {:ok, Expression.new(op, expression, nested_expression)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_predicates(value, field, context) when not is_list(value) do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(values, attr, context) do
    data_layer_predicates =
      Map.get(
        Ash.data_layer_filters(context.resource),
        Ash.Type.storage_type(attr.type),
        []
      )

    if Keyword.keyword?(values) do
      Enum.reduce_while(values, {:ok, nil}, fn {key, value}, {:ok, expression} ->
        case @built_in_predicates[key] || data_layer_predicates[key] do
          value when value in [nil, []] ->
            {:halt, {:error, "No such filter predicate: #{inspect(key)}"}}

          predicate_module ->
            case Predicate.new(
                   context.resource,
                   attr,
                   predicate_module,
                   value,
                   context.relationship_path
                 ) do
              {:ok, predicate} ->
                {:cont, {:ok, Expression.new(:and, expression, predicate)}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
        end
      end)
    else
      {:halt, {:error, "Invalid filter expression: #{inspect(values)}"}}
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    @custom_colors [
      number: :cyan
    ]

    def inspect(
          %{expression: expression},
          opts
        ) do
      opts = %{opts | syntax_colors: Keyword.merge(opts.syntax_colors, @custom_colors)}
      concat(["#Ash.Filter<", to_doc(expression, opts), ">"])
    end
  end
end
