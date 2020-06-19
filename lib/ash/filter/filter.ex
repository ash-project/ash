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

  def relationship_paths(nil), do: []
  def relationship_paths(%{expression: nil}), do: []

  def relationship_paths(filter) do
    filter.expression
    |> do_relationship_paths()
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

  # def primary_key_filter?(nil), do: false

  # def primary_key_filter?(filter) do
  #   pkey = Ash.primary_key(filter.resource)
  #   cleared_pkey_filter = Enum.map(pkey, fn key -> {key, nil} end)

  #   case cleared_pkey_filter do
  #     [] ->
  #       false

  #     cleared_pkey_filter ->
  #       case parse(filter.api, filter.resource, cleared_pkey_filter) do
  #         {:ok, parsed_cleared_pkey_filter} ->
  #           cleared_candidate_filter = clear_primary_key(filter, pkey)

  #           strict_subset_of?(parsed_cleared_pkey_filter, cleared_candidate_filter)

  #         _ ->
  #           false
  #       end
  #   end
  # end

  # defp clear_primary_key(filter, pkey) do
  #   map(filter, fn
  #     %Predicate{attribute: %{name: name}, predicate: %mod{}} = predicate
  #     when mod in [Eq, In] ->
  #       if name in pkey do
  #         %{
  #           predicate
  #           | predicate: %Predicate{
  #               attribute: Ash.attribute(filter.resource, name),
  #               predicate: %Eq{field: name, value: nil}
  #             }
  #         }
  #       else
  #         predicate
  #       end

  #     other ->
  #       other
  #   end)
  # end

  def relationship_filter_request_paths(filter) do
    filter
    |> relationship_paths()
    |> Enum.map(&[:filter, &1])
  end

  def read_requests(nil), do: []

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
    acc = func.(expression, acc)
    do_reduce(expression, acc, func)
  end

  def reduce(expression, acc, func) do
    acc = func.(expression, acc)
    do_reduce(expression, acc, func)
  end

  def do_reduce(expression, acc, func) do
    case expression do
      %Expression{left: left, right: right} ->
        acc = func.(right, acc)
        acc = func.(left, acc)
        do_reduce(right, do_reduce(left, acc, func), func)

      %Not{expression: not_expr} ->
        acc = func.(not_expr, acc)

        do_reduce(not_expr, acc, func)

      other ->
        func.(other, acc)
    end
  end

  defp filter_expression_by_relationship_path(filter, path) do
    %__MODULE__{
      api: filter.api,
      resource: Ash.related(filter.resource, path),
      expression: do_filter_expression_by_relationship_path(filter.expression, path)
    }
  end

  defp do_filter_expression_by_relationship_path(
         %Expression{op: op, left: left, right: right},
         path
       ) do
    new_left = do_filter_expression_by_relationship_path(left, path)
    new_right = do_filter_expression_by_relationship_path(right, path)

    Expression.new(op, new_left, new_right)
  end

  defp do_filter_expression_by_relationship_path(%Not{expression: expression} = not_expr, path) do
    new_expression = do_filter_expression_by_relationship_path(expression, path)
    %{not_expr | expression: new_expression}
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

  defp do_relationship_paths(%Predicate{relationship_path: []}) do
    []
  end

  defp do_relationship_paths(%Predicate{relationship_path: path}) do
    {path}
  end

  defp do_relationship_paths(%Expression{left: left, right: right}) do
    [do_relationship_paths(left), do_relationship_paths(right)]
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

  defp add_expression_part({op, nested_statements}, context, expression) when op in [:or, :and] do
    case parse_and_join(nested_statements, op, context) do
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
