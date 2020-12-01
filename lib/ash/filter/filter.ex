defmodule Ash.Filter do
  alias Ash.Actions.SideLoad
  alias Ash.Engine.Request

  alias Ash.Error.Query.{
    AggregatesNotSupported,
    InvalidFilterValue,
    NoSuchAttributeOrRelationship,
    NoSuchFilterPredicate,
    ReadActionRequired
  }

  alias Ash.Query.Function.IsNil

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    LessThan,
    LessThanOrEqual,
    NotEq
  }

  alias Ash.Query.{Expression, Not, Ref}
  alias Ash.Query.{Aggregate, Function, Operator}

  @functions [
    IsNil
  ]

  @operators [
    Ash.Query.Operator.IsNil,
    Eq,
    NotEq,
    In,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual
  ]

  @builtins @functions ++ @operators

  @operator_aliases [
    eq: Eq,
    equals: Eq,
    not_eq: NotEq,
    not_equals: NotEq,
    gt: GreaterThan,
    greater_than: GreaterThan,
    lt: LessThan,
    less_than: LessThan,
    gte: GreaterThanOrEqual,
    greater_than_or_equal: GreaterThanOrEqual,
    lte: LessThanOrEqual,
    less_than_or_equal: LessThanOrEqual
  ]

  @moduledoc """
  The representation of a filter in Ash.

  Ash filters are stored as nested `Ash.Query.Expression{}` and `%Ash.Query.Not{}` structs,
  terminating in an operator or a function struct. An expression is simply a boolean operator
  and the left and right hand side of that operator.

  ## Filter Templates

  Filter templates are simplified fielter statements (they only support atom keys), that have substitutions in them.
  Currently, the substitutions are `{:_actor, :field}` and `{:_actor, :_primary_key}`

  You can pass a filter template to `build_filter_from_template/2` with an actor, and it will return the new result

  Additionally, you can ask if the filter template contains an actor reference via `template_references_actor?/1`

  ## Writing a filter

  ### Built In Predicates

  #{Enum.map_join(@operators, "\n", &"* `#{&1.operator()}`")}
  #{
    Enum.map_join(@operator_aliases, "\n", fn {key, val} ->
      "* `#{key}` (alias for `#{val.operator()}`)"
    end)
  }

  ### Expression syntax

  The expression syntax ultimately just builds the keyword list style filter, but with lots of conveniences that
  would be very annoying to do manually.

  Examples

  ```elixir
  Ash.Query.filter(resource, name == "Zardoz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name == "Doz")
  Ash.Query.filter(resource, first_name == "Zar" and last_name in ["Doz", "Daz"] and high_score > 10)
  Ash.Query.filter(resource, first_name == "Zar" or last_name == "Doz" or (high_score > 10 and high_score < -10))
  ```

  ### Keyword list syntax

  A filter is a nested keyword list (with some exceptions, like `true` for everything and `false` for nothing).

  The key is the "predicate" (A.K.A condition) and the value is the parameter. You can use `and` and `or` to create
  nested filters. Datalayers can expose custom predicates. Eventually, you will be able to define your own custom
  predicates, which will be a mechanism for you to attach complex filters supported by the data layer to your queries.

  ** Important **
  In a given keyword list, all predicates are considered to be "ands". So `[or: [first_name: "Tom", last_name: "Bombadil"]]` doesn't
  mean 'First name == "tom" or last_name == "bombadil"'. To say that, you want to provide a list of filters,
  like so: `[or: [[first_name: "Tom"], [last_name: "Bombadil"]]]`

  Some example filters:

  ```elixir
  Ash.Query.filter(resource, [name: "Zardoz"]))
  Ash.Query.filter(resource, [first_name: "Zar", last_name: "Doz"])
  Ash.Query.filter(resource, [first_name: "Zar", last_name: [in: ["Doz", "Daz"]], high_score: [greater_than: 10]])
  Ash.Query.filter(resource, [or: [
    [first_name: "Zar"],
    [last_name: "Doz"],
    [or: [
      [high_score: [greater_than: 10]]],
      [high_score: [less_than: -10]]
    ]
  ]])
  ```
  """

  @builtin_operators Enum.map(@operators, &{&1.operator(), &1}) ++ @operator_aliases
  @builtin_functions Enum.map(@functions, &{&1.name(), &1})

  @string_builtin_operators Enum.into(@builtin_operators, %{}, fn {key, value} ->
                              {to_string(key), value}
                            end)

  @string_builtin_functions Enum.into(@builtin_functions, %{}, fn {key, value} ->
                              {to_string(key), value}
                            end)

  defstruct [:resource, :expression]

  @type t :: %__MODULE__{}

  def builtins, do: @builtins
  def builtin_functions, do: @functions
  def builtin_operators, do: @operators

  defmodule Simple do
    @moduledoc "Represents a simplified filter, with a simple list of predicates"
    defstruct [:resource, :predicates]

    defmodule Not do
      @moduledoc "A negated predicate"
      defstruct [:predicate]
    end
  end

  def parse!(resource, statement, aggregates \\ %{}) do
    case parse(resource, statement, aggregates) do
      {:ok, filter} ->
        filter

      {:error, error} ->
        raise error
    end
  end

  def parse(resource, statement, aggregates \\ %{}) do
    context = %{
      resource: resource,
      relationship_path: [],
      aggregates: aggregates
    }

    case parse_expression(statement, context) do
      {:ok, expression} ->
        {:ok, %__MODULE__{expression: expression, resource: resource}}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc "transform an expression based filter to a simple filter, which is just a list of predicates"
  def to_simple_filter(%{resource: resource, expression: expression}) do
    predicates = get_predicates(expression)

    %Simple{resource: resource, predicates: predicates}
  end

  @doc "Replace any actor value references in a template with the values from a given actor"
  def build_filter_from_template(template, actor) do
    walk_filter_template(template, fn
      {:_actor, :_primary_key} ->
        if actor do
          Map.take(actor, Ash.Resource.primary_key(actor.__struct__))
        else
          false
        end

      {:_actor, field} ->
        Map.get(actor || %{}, field)

      other ->
        other
    end)
  end

  @doc "Whether or not a given template contains an actor reference"
  def template_references_actor?({:_actor, _}), do: true

  def template_references_actor?(filter) when is_list(filter) do
    Enum.any?(filter, &template_references_actor?/1)
  end

  def template_references_actor?(filter) when is_map(filter) do
    Enum.any?(fn {key, value} ->
      template_references_actor?(key) || template_references_actor?(value)
    end)
  end

  def template_references_actor?(tuple) when is_tuple(tuple) do
    Enum.any?(Tuple.to_list(tuple), &template_references_actor?/1)
  end

  def template_references_actor?(_), do: false

  defp walk_filter_template(filter, mapper) when is_list(filter) do
    case mapper.(filter) do
      ^filter ->
        Enum.map(filter, &walk_filter_template(&1, mapper))

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(filter, mapper) when is_map(filter) do
    case mapper.(filter) do
      ^filter ->
        Enum.into(filter, %{}, &walk_filter_template(&1, mapper))

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(tuple, mapper) when is_tuple(tuple) do
    case mapper.(tuple) do
      ^tuple ->
        tuple
        |> Tuple.to_list()
        |> Enum.map(&walk_filter_template(&1, mapper))
        |> List.to_tuple()

      other ->
        walk_filter_template(other, mapper)
    end
  end

  defp walk_filter_template(value, mapper), do: mapper.(value)

  defp get_predicates(expr, acc \\ [])

  defp get_predicates(true, acc), do: acc
  defp get_predicates(false, _), do: false
  defp get_predicates(_, false), do: false

  defp get_predicates(%Expression{op: :and, left: left, right: right}, acc) do
    acc = get_predicates(left, acc)
    get_predicates(right, acc)
  end

  defp get_predicates(%Not{expression: expression}, acc) do
    expression
    |> get_predicates()
    |> Enum.reduce(acc, fn predicate, acc ->
      [%Simple.Not{predicate: predicate} | acc]
    end)
  end

  defp get_predicates(%{__predicate__?: true} = predicate, acc), do: [predicate | acc]

  def used_aggregates(filter) do
    filter
    |> list_predicates()
    |> Enum.flat_map(fn
      %{__operator__?: true, left: left, right: right} ->
        [left, right]
        |> Enum.filter(fn
          %Ref{attribute: %Aggregate{}} ->
            true

          _ ->
            false
        end)
        |> Enum.map(& &1.attribute)

      %{__function__?: true, arguments: arguments} ->
        arguments
        |> Enum.filter(fn
          %Ash.Query.Ref{attribute: %Aggregate{}} ->
            true

          _ ->
            false
        end)
        |> Enum.map(& &1.attribute)
    end)
  end

  def run_other_data_layer_filters(api, resource, %{expression: expression} = filter) do
    case do_run_other_data_layer_filters(expression, api, resource) do
      {:ok, new_expression} -> {:ok, %{filter | expression: new_expression}}
      {:error, error} -> {:error, error}
    end
  end

  def run_other_data_layer_filters(_, _, filter) when filter in [nil, true, false],
    do: {:ok, filter}

  defp do_run_other_data_layer_filters(
         %Expression{op: :or, left: left, right: right},
         api,
         resource
       ) do
    with {:ok, left} <- do_run_other_data_layer_filters(left, api, resource),
         {:ok, right} <- do_run_other_data_layer_filters(right, api, resource) do
      {:ok, Expression.optimized_new(:or, left, right)}
    end
  end

  defp do_run_other_data_layer_filters(%Expression{op: :and} = expression, api, resource) do
    expression
    |> relationship_paths(:ands_only)
    |> filter_paths_that_change_data_layers(resource)
    |> case do
      [] ->
        {:ok, expression}

      paths ->
        paths
        |> do_run_other_data_layer_filter_paths(expression, resource, api)
        |> case do
          {:ok, result} -> do_run_other_data_layer_filters(result, api, resource)
          {:error, error} -> {:error, error}
        end
    end
    |> case do
      {:ok, %Expression{op: :and, left: left, right: right}} ->
        with {:ok, new_left} <- do_run_other_data_layer_filters(left, api, resource),
             {:ok, new_right} <- do_run_other_data_layer_filters(right, api, resource) do
          {:ok, Expression.optimized_new(:and, new_left, new_right)}
        end
    end
  end

  defp do_run_other_data_layer_filters(%Not{expression: expression}, api, resource) do
    case do_run_other_data_layer_filters(expression, api, resource) do
      {:ok, expr} -> {:ok, Not.new(expr)}
      {:error, error} -> {:error, error}
    end
  end

  defp do_run_other_data_layer_filters(%{__predicate__?: true} = predicate, api, resource) do
    predicate
    |> relationship_paths(:ands_only)
    |> filter_paths_that_change_data_layers(resource)
    |> Enum.find_value(fn path ->
      case split_expression_by_relationship_path(predicate, path) do
        {nil, _} ->
          nil

        {for_path, nil} ->
          {path, for_path}
      end
    end)
    |> case do
      nil ->
        {:ok, predicate}

      {path, new_predicate} ->
        relationship = Ash.Resource.relationship(resource, path)

        fetch_related_data(resource, path, new_predicate, api, relationship)
    end
  end

  defp do_run_other_data_layer_filters(other, _api, _resource), do: {:ok, other}

  defp do_run_other_data_layer_filter_paths(paths, expression, resource, api) do
    Enum.reduce_while(paths, {:ok, expression}, fn path, {:ok, expression} ->
      {for_path, without_path} = split_expression_by_relationship_path(expression, path)

      relationship = Ash.Resource.relationship(resource, path)

      query =
        relationship.destination
        |> Ash.Query.new(api)
        |> Map.put(:filter, %__MODULE__{
          expression: for_path,
          resource: relationship.destination
        })

      case filter_related_in(query, relationship, :lists.droplast(path)) do
        {:ok, new_predicate} ->
          {:cont, {:ok, Expression.optimized_new(:and, without_path, new_predicate)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp fetch_related_data(
         resource,
         path,
         new_predicate,
         api,
         %{type: :many_to_many, join_relationship: join_relationship, through: through} =
           relationship
       ) do
    if Ash.Resource.data_layer(through) == Ash.Resource.data_layer(resource) &&
         Ash.Resource.data_layer_can?(resource, {:join, through}) do
      filter = %__MODULE__{
        resource: relationship.destination,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(api)
      |> Ash.Query.do_filter(filter)
      |> filter_related_in(
        relationship,
        :lists.droplast(path) ++ [join_relationship]
      )
    else
      filter = %__MODULE__{
        resource: through,
        expression: new_predicate
      }

      relationship.destination
      |> Ash.Query.new(api)
      |> Ash.Query.do_filter(filter)
      |> Ash.Actions.Read.unpaginated_read()
      |> case do
        {:ok, results} ->
          relationship.through
          |> Ash.Query.new(api)
          |> Ash.Query.do_filter([
            {relationship.destination_field_on_join_table,
             in: Enum.map(results, &Map.get(&1, relationship.destination_field))}
          ])
          |> filter_related_in(
            Ash.Resource.relationship(resource, join_relationship),
            :lists.droplast(path)
          )

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp fetch_related_data(
         _resource,
         path,
         new_predicate,
         api,
         relationship
       ) do
    filter = %__MODULE__{
      resource: relationship.destination,
      expression: new_predicate
    }

    relationship.destination
    |> Ash.Query.new(api)
    |> Ash.Query.do_filter(filter)
    |> filter_related_in(relationship, :lists.droplast(path))
  end

  defp filter_related_in(query, relationship, path) do
    case Ash.Actions.Read.unpaginated_read(query) do
      {:error, error} ->
        {:error, error}

      {:ok, records} ->
        records_to_expression(
          records,
          relationship,
          path
        )
    end
  end

  defp records_to_expression([], _, _), do: {:ok, false}

  defp records_to_expression([single_record], relationship, path) do
    Ash.Query.Operator.new(
      Eq,
      %Ref{
        relationship_path: path,
        resource: relationship.source,
        attribute: Ash.Resource.attribute(relationship.source, relationship.source_field)
      },
      Map.get(single_record, relationship.destination_field)
    )
  end

  defp records_to_expression(records, relationship, path) do
    Enum.reduce_while(records, {:ok, nil}, fn record, {:ok, expression} ->
      case records_to_expression([record], relationship, path) do
        {:ok, operator} ->
          {:cont, {:ok, Expression.optimized_new(:and, expression, operator)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
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
    relationship = Ash.Resource.relationship(resource, relationship)

    if relationship.type == :many_to_many do
      if Ash.Resource.data_layer_can?(resource, {:join, relationship.through}) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    else
      if Ash.Resource.data_layer_can?(resource, {:join, relationship.destination}) do
        shortest_path_to_changed_data_layer(relationship.destination, rest, [
          relationship.name | acc
        ])
      else
        {:ok, Enum.reverse([relationship.name | acc])}
      end
    end
  end

  def put_at_path(value, []), do: value
  def put_at_path(value, [key | rest]), do: [{key, put_at_path(value, rest)}]

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

  def add_to_filter!(base, addition, op \\ :and, aggregates \\ %{}) do
    case add_to_filter(base, addition, op, aggregates) do
      {:ok, value} ->
        value

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  def add_to_filter(base, addition, op \\ :and, aggregates \\ %{})

  def add_to_filter(nil, %__MODULE__{} = addition, _, _), do: {:ok, addition}

  def add_to_filter(
        %__MODULE__{} = base,
        %__MODULE__{} = addition,
        op,
        _
      ) do
    {:ok,
     %{
       base
       | expression: Expression.optimized_new(op, base.expression, addition.expression)
     }}
  end

  def add_to_filter(%__MODULE__{} = base, statement, op, aggregates) do
    case parse(base.resource, statement, aggregates) do
      {:ok, filter} -> add_to_filter(base, filter, op, aggregates)
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

  def read_requests(_, nil), do: {:ok, []}

  def read_requests(api, filter) do
    filter
    |> Ash.Filter.relationship_paths()
    |> Enum.map(fn path ->
      {path, filter_expression_by_relationship_path(filter, path, true)}
    end)
    |> Enum.reduce_while({:ok, []}, fn {path, scoped_filter}, {:ok, requests} ->
      %{resource: resource} = scoped_filter

      with %{errors: []} = query <- Ash.Query.new(resource, api),
           %{errors: []} = query <- Ash.Query.do_filter(query, scoped_filter),
           {:action, action} when not is_nil(action) <-
             {:action, Ash.Resource.primary_action(resource, :read)} do
        request =
          Request.new(
            resource: resource,
            api: api,
            query:
              Request.resolve(
                [[:data, :authorization_filter]],
                fn %{
                     data: %{
                       authorization_filter: authorization_filter
                     }
                   } ->
                  if authorization_filter do
                    relationship =
                      Ash.Resource.relationship(
                        resource,
                        List.first(path)
                      )

                    case SideLoad.reverse_relationship_path(
                           relationship,
                           tl(path)
                         ) do
                      :error ->
                        {:ok, query}

                      {:ok, reverse_relationship} ->
                        filter = put_at_path(authorization_filter, reverse_relationship)
                        {:ok, Ash.Query.do_filter(query, filter)}
                    end
                  else
                    {:ok, query}
                  end
                end
              ),
            async?: false,
            path: [:filter, path],
            strict_check_only?: true,
            action: action,
            name: "authorize filter #{Enum.join(path, ".")}",
            data: []
          )

        {:cont, {:ok, [request | requests]}}
      else
        {:error, error} -> {:halt, {:error, error}}
        %{errors: errors} -> {:halt, {:error, errors}}
        {:action, nil} -> {:halt, {:error, ReadActionRequired.exception(resource: resource)}}
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

      %{__operator__?: true, left: left, right: right} = op ->
        %{op | left: do_map(left, func), right: do_map(right, func)}

      %{__function__?: true, arguments: arguments} = func ->
        %{func | arguments: Enum.map(arguments, &do_map(&1, func))}

      other ->
        func.(other)
    end
  end

  def list_predicates(%__MODULE__{expression: expression}) do
    list_predicates(expression)
  end

  def list_predicates(expression) do
    case expression do
      %Expression{left: left, right: right} ->
        list_predicates(left) ++ list_predicates(right)

      %Not{expression: not_expr} ->
        list_predicates(not_expr)

      %{__predicate__?: true} = pred ->
        [pred]

      _ ->
        []
    end
  end

  def filter_expression_by_relationship_path(filter, path, scope? \\ false) do
    %__MODULE__{
      resource: Ash.Resource.related(filter.resource, path),
      expression: do_filter_expression_by_relationship_path(filter.expression, path, scope?)
    }
  end

  defp split_expression_by_relationship_path(%{expression: expression}, path) do
    split_expression_by_relationship_path(expression, path)
  end

  defp split_expression_by_relationship_path(
         %Expression{op: op, left: left, right: right},
         path
       ) do
    {new_for_path_left, new_without_path_left} = split_expression_by_relationship_path(left, path)

    {new_for_path_right, new_without_path_right} =
      split_expression_by_relationship_path(right, path)

    {Expression.optimized_new(op, new_for_path_left, new_for_path_right),
     Expression.optimized_new(op, new_without_path_left, new_without_path_right)}
  end

  defp split_expression_by_relationship_path(%Not{expression: expression}, path) do
    {new_for_path, new_without_path} = split_expression_by_relationship_path(expression, path)
    {Not.new(new_for_path), Not.new(new_without_path)}
  end

  defp split_expression_by_relationship_path(
         %{
           __operator__?: true,
           left: %Ref{relationship_path: predicate_path} = left,
           right: %Ref{relationship_path: predicate_path}
         } = predicate,
         path
       ) do
    if List.starts_with?(predicate_path, path) do
      new_path = Enum.drop(predicate_path, length(path))

      {%{
         predicate
         | left: %{
             left
             | relationship_path: new_path
           }
       }, nil}
    else
      {nil, predicate}
    end
  end

  defp split_expression_by_relationship_path(
         %{__operator__?: true, right: %Ref{}},
         _path
       ) do
    raise "Refs not currently supported on the right side of operators with different relationship paths"
  end

  defp split_expression_by_relationship_path(
         %{__operator__?: true, left: %Ref{relationship_path: predicate_path} = ref} = predicate,
         path
       ) do
    if List.starts_with?(predicate_path, path) do
      new_path = Enum.drop(predicate_path, length(path))

      {%{predicate | left: %{ref | relationship_path: new_path}}, nil}
    else
      {nil, predicate}
    end
  end

  defp split_expression_by_relationship_path(
         %{__function__?: true, arguments: arguments} = func,
         path
       ) do
    arguments
    |> Enum.filter(&match?(%Ref{}, &1))
    |> Enum.map(& &1.relationship_path)
    |> Enum.uniq()
    |> case do
      [] ->
        {func, func}

      [predicate_path] ->
        if List.starts_with?(predicate_path, path) do
          new_args =
            Enum.map(arguments, fn
              %Ref{relationship_path: predicate_path} = ref ->
                %{ref | relationship_path: Enum.drop(predicate_path, length(path))}

              arg ->
                arg
            end)

          {%{func | arguments: new_args}, nil}
        else
          {nil, func}
        end

      _ ->
        raise "Refs for multiple relationship paths not supported in a single function call"
    end
  end

  defp do_filter_expression_by_relationship_path(
         %Expression{op: op, left: left, right: right},
         path,
         scope?
       ) do
    new_left = do_filter_expression_by_relationship_path(left, path, scope?)
    new_right = do_filter_expression_by_relationship_path(right, path, scope?)

    Expression.optimized_new(op, new_left, new_right)
  end

  defp do_filter_expression_by_relationship_path(%Not{expression: expression}, path, scope?) do
    new_expression = do_filter_expression_by_relationship_path(expression, path, scope?)
    Not.new(new_expression)
  end

  defp do_filter_expression_by_relationship_path(
         %{__operator__?: true, left: left, right: right} = op,
         path,
         scope?
       ) do
    if scope? do
      %{op | left: scope_ref(left, path), right: scope_ref(right, path)}
    else
      [left, right]
      |> Enum.filter(&match?(%Ref{}, &1))
      |> Enum.any?(&List.starts_with?(&1.relationship_path, path))
      |> case do
        true ->
          nil

        false ->
          op
      end
    end
  end

  defp do_filter_expression_by_relationship_path(
         %{__function__?: true, arguments: arguments} = func,
         path,
         scope?
       ) do
    if scope? do
      %{func | arguments: Enum.map(arguments, &scope_ref(&1, path))}
    else
      arguments
      |> Enum.filter(&match?(%Ref{}, &1))
      |> Enum.any?(&List.starts_with?(&1.relationship_path, path))
      |> case do
        true ->
          nil

        false ->
          func
      end
    end
  end

  defp do_filter_expression_by_relationship_path(other, _path, _scope) do
    other
  end

  defp scope_ref(%Ref{} = ref, path) do
    if List.starts_with?(ref.relationship_path, path) do
      %{ref | relationship_path: Enum.drop(ref.relationship_path, Enum.count(path))}
    else
      ref
    end
  end

  defp scope_ref(other, _), do: other

  defp do_relationship_paths(%Ref{relationship_path: path}, _) when path != [] do
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

  defp do_relationship_paths(%{__operator__?: true, left: left, right: right}, kind) do
    [do_relationship_paths(left, kind), do_relationship_paths(right, kind)]
  end

  defp do_relationship_paths(%{__operator__?: true, arguments: arguments}, kind) do
    Enum.map(arguments, &do_relationship_paths(&1, kind))
  end

  defp do_relationship_paths(_, _), do: []

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
    do: {:ok, Expression.optimized_new(:and, expression, boolean)}

  defp add_expression_part(%__MODULE__{expression: adding_expression}, context, expression) do
    {:ok,
     Expression.optimized_new(
       :and,
       expression,
       add_to_predicate_path(adding_expression, context)
     )}
  end

  defp add_expression_part(%_{} = record, context, expression) do
    pkey_filter =
      record
      |> Map.take(Ash.Resource.primary_key(context.resource))
      |> Map.to_list()

    add_expression_part(pkey_filter, context, expression)
  end

  defp add_expression_part({not_key, nested_statement}, context, expression)
       when not_key in [:not, "not"] do
    case parse_expression(nested_statement, context) do
      {:ok, nested_expression} ->
        {:ok, Expression.optimized_new(:and, expression, Not.new(nested_expression))}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({or_key, nested_statements}, context, expression)
       when or_key in [:or, "or"] do
    with {:ok, nested_expression} <- parse_and_join(nested_statements, :or, context),
         :ok <- validate_data_layers_support_boolean_filters(nested_expression) do
      {:ok, Expression.optimized_new(:and, expression, nested_expression)}
    end
  end

  defp add_expression_part({and_key, nested_statements}, context, expression)
       when and_key in [:and, "and"] do
    case parse_and_join(nested_statements, :and, context) do
      {:ok, nested_expression} ->
        {:ok, Expression.optimized_new(:and, expression, nested_expression)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part({%Ref{} = ref, nested_statement}, context, expression) do
    new_context = %{
      relationship_path: ref.relationship_path,
      resource: Ash.Resource.related(context.resource, ref.relationship_path),
      aggregates: context.aggregates
    }

    add_expression_part({ref.attribute.name, nested_statement}, new_context, expression)
  end

  defp add_expression_part({field, nested_statement}, context, expression)
       when is_atom(field) or is_binary(field) do
    aggregates =
      Enum.flat_map(context.aggregates, fn {key, _} ->
        [key, to_string(key)]
      end)

    cond do
      function_module = get_function(field, Ash.Resource.data_layer_functions(context.resource)) ->
        with {:ok, args} <-
               hydrate_refs(List.wrap(nested_statement), context.resource, context.aggregates),
             {:ok, function} <-
               Function.new(
                 function_module,
                 args,
                 %Ref{
                   relationship_path: context.relationship_path,
                   resource: context.resource
                 }
               ) do
          {:ok, Expression.optimized_new(:and, expression, function)}
        end

      rel = Ash.Resource.relationship(context.resource, field) ->
        context =
          context
          |> Map.update!(:relationship_path, fn path -> path ++ [rel.name] end)
          |> Map.put(:resource, rel.destination)

        if is_list(nested_statement) || is_map(nested_statement) do
          case parse_expression(nested_statement, context) do
            {:ok, nested_expression} ->
              {:ok, Expression.optimized_new(:and, expression, nested_expression)}

            {:error, error} ->
              {:error, error}
          end
        else
          with [field] <- Ash.Resource.primary_key(context.resource),
               attribute <- Ash.Resource.attribute(context.resource, field),
               {:ok, casted} <-
                 Ash.Type.cast_input(attribute.type, nested_statement) do
            add_expression_part({field, casted}, context, expression)
          else
            _other ->
              {:error,
               InvalidFilterValue.exception(
                 value: inspect(nested_statement),
                 message:
                   "A single value must be castable to the primary key of the resource: #{
                     inspect(context.resource)
                   }"
               )}
          end
        end

      attr = Ash.Resource.attribute(context.resource, field) ->
        case parse_predicates(nested_statement, attr, context) do
          {:ok, nested_statement} ->
            {:ok, Expression.optimized_new(:and, expression, nested_statement)}

          {:error, error} ->
            {:error, error}
        end

      field in aggregates ->
        field =
          if is_binary(field) do
            String.to_existing_atom(field)
          else
            field
          end

        add_aggregate_expression(context, nested_statement, field, expression)

      (op_module = get_operator(field, Ash.Resource.data_layer_operators(context.resource))) &&
          match?([_, _ | _], nested_statement) ->
        with {:ok, [left, right]} <-
               hydrate_refs(nested_statement, context.resource, context.aggregates),
             {:ok, operator} <- Operator.new(op_module, left, right) do
          {:ok, Expression.optimized_new(:and, expression, operator)}
        end

      true ->
        {:error,
         NoSuchAttributeOrRelationship.exception(
           attribute_or_relationship: field,
           resource: context.resource
         )}
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
      {:ok, new_expression} ->
        {:ok, Expression.optimized_new(:and, expression, new_expression)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_expression_part(value, context, expression) when is_list(value) do
    Enum.reduce_while(value, {:ok, expression}, fn value, {:ok, expression} ->
      case add_expression_part(value, context, expression) do
        {:ok, expression} -> {:cont, {:ok, expression}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp add_expression_part(value, _, _) do
    {:error, InvalidFilterValue.exception(value: value)}
  end

  defp hydrate_refs(list, resource, aggregates) do
    list
    |> Enum.reduce_while({:ok, []}, fn
      %Ref{attribute: attribute} = ref, {:ok, acc} when is_atom(attribute) ->
        case Ash.Resource.related(resource, ref.relationship_path) do
          nil ->
            {:halt, {:error, "Invalid reference #{inspect(ref)}"}}

          related ->
            do_hydrate_ref(ref, attribute, related, aggregates, acc)
        end

      other, {:ok, acc} ->
        {:cont, {:ok, [other | acc]}}
    end)
    |> case do
      {:ok, refs} -> {:ok, Enum.reverse(refs)}
      {:error, error} -> {:error, error}
    end
  end

  defp do_hydrate_ref(ref, field, related, aggregates, acc) do
    cond do
      Map.has_key?(aggregates, field) ->
        {:cont, {:ok, [%{ref | attribute: Map.get(aggregates, field)} | acc]}}

      attribute = Ash.Resource.attribute(related, field) ->
        {:cont, {:ok, [%{ref | attribute: attribute} | acc]}}

      relationship = Ash.Resource.relationship(related, field) ->
        case Ash.Resource.primary_key(relationship.destination) do
          [key] ->
            new_ref = %{
              ref
              | relationship_path: ref.relationship_path ++ [relationship.name],
                attribute: Ash.Resource.attribute(relationship.destination, key)
            }

            {:cont, {:ok, [new_ref | acc]}}

          _ ->
            {:halt, {:error, "Invalid reference #{inspect(ref)}"}}
        end

      true ->
        {:halt, {:error, "Invalid reference #{inspect(ref)}"}}
    end
  end

  defp add_aggregate_expression(context, nested_statement, field, expression) do
    if Ash.Resource.data_layer_can?(context.resource, :aggregate_filter) do
      case parse_predicates(nested_statement, Map.get(context.aggregates, field), context) do
        {:ok, nested_statement} ->
          {:ok, Expression.optimized_new(:and, expression, nested_statement)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, AggregatesNotSupported.exception(resource: context.resource, feature: "filtering")}
    end
  end

  defp validate_data_layers_support_boolean_filters(%Expression{
         op: :or,
         left: left,
         right: right
       }) do
    left_resources =
      left
      |> map(fn
        %Ref{} = ref ->
          [ref.resource]

        _ ->
          []
      end)
      |> List.flatten()
      |> Enum.uniq()

    right_resources =
      right
      |> map(fn
        %Ref{} = ref ->
          [ref.resource]

        _ ->
          []
      end)
      |> List.flatten()
      |> Enum.uniq()

    left_resources
    |> Enum.filter(&(&1 in right_resources))
    |> Enum.reduce_while(:ok, fn resource, :ok ->
      if Ash.Resource.data_layer_can?(resource, :boolean_filter) do
        {:cont, :ok}
      else
        {:halt, {:error, "Data layer for #{resource} does not support boolean filters"}}
      end
    end)
  end

  defp validate_data_layers_support_boolean_filters(_), do: :ok

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

      %{__operator__?: true, left: left, right: right} = op ->
        left = add_to_ref_path(left, context.relationship_path)
        right = add_to_ref_path(right, context.relationship_path)
        %{op | left: left, right: right}

      %{__function__?: true, arguments: args} = func ->
        %{func | arguments: Enum.map(args, &add_to_ref_path(&1, context.relationship_path))}

      other ->
        other
    end
  end

  defp add_to_ref_path(%Ref{relationship_path: relationship_path} = ref, to_add) do
    %{ref | relationship_path: to_add ++ relationship_path}
  end

  defp add_to_ref_path(other, _), do: other

  defp parse_and_join(statements, op, context) do
    Enum.reduce_while(statements, {:ok, nil}, fn statement, {:ok, expression} ->
      case parse_expression(statement, context) do
        {:ok, nested_expression} ->
          {:cont, {:ok, Expression.optimized_new(op, expression, nested_expression)}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
  end

  defp parse_predicates(value, field, context) when not is_list(value) and not is_map(value) do
    parse_predicates([eq: value], field, context)
  end

  defp parse_predicates(values, attr, context) do
    if is_map(values) || Keyword.keyword?(values) do
      Enum.reduce_while(values, {:ok, nil}, fn
        {:not, value}, {:ok, expression} ->
          case parse_predicates(List.wrap(value), attr, context) do
            {:ok, not_expression} ->
              {:cont,
               {:ok, Expression.optimized_new(:and, expression, %Not{expression: not_expression})}}

            {:error, error} ->
              {:halt, {:error, error}}
          end

        {key, value}, {:ok, expression} ->
          case get_operator(key, Ash.Resource.data_layer_operators(context.resource)) do
            nil ->
              error = NoSuchFilterPredicate.exception(key: key, resource: context.resource)
              {:halt, {:error, error}}

            operator_module ->
              left = %Ref{
                attribute: attr,
                relationship_path: context.relationship_path,
                resource: context.resource
              }

              with {:ok, [left, right]} <-
                     hydrate_refs([left, value], context.resource, context.aggregates),
                   {:ok, operator} <- Operator.new(operator_module, left, right) do
                if is_boolean(operator) do
                  {:cont, {:ok, operator}}
                else
                  if Ash.Resource.data_layer_can?(context.resource, {:filter_operator, operator}) do
                    {:cont, {:ok, Expression.optimized_new(:and, expression, operator)}}
                  else
                    {:halt,
                     {:error, "data layer does not support the operator #{inspect(operator)}"}}
                  end
                end
              else
                {:error, error} -> {:halt, {:error, error}}
              end
          end
      end)
    else
      error = InvalidFilterValue.exception(value: values)
      {:error, error}
    end
  end

  defp get_function(key, data_layer_functions) when is_atom(key) do
    @builtin_functions[key] || Enum.find(data_layer_functions, &(&1.name() == key))
  end

  defp get_function(key, data_layer_functions) when is_binary(key) do
    Map.get(@string_builtin_functions, key) ||
      Enum.find(data_layer_functions, &(&1.name() == key))
  end

  defp get_operator(key, data_layer_operators) when is_atom(key) do
    @builtin_operators[key] || Enum.find(data_layer_operators, &(&1.operator() == key))
  end

  defp get_operator(key, data_layer_operators) when is_binary(key) do
    Map.get(@string_builtin_operators, key) ||
      Enum.find(data_layer_operators, &(&1.name() == key))
  end

  defp get_operator(_, _), do: nil

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
