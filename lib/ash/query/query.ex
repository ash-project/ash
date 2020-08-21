defmodule Ash.Query do
  @moduledoc """
  Utilties around constructing/manipulating ash queries.

  Ash queries are used for read actions and side loads, and ultimately
  map to queries to a resource's data layer.
  """
  defstruct [
    :api,
    :resource,
    :filter,
    :data_layer_query,
    aggregates: %{},
    side_load: [],
    data_layer_context: %{},
    sort: [],
    limit: nil,
    offset: 0,
    errors: [],
    valid?: true
  ]

  @type t :: %__MODULE__{}

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(query, opts) do
      sort? = query.sort != []
      side_load? = query.side_load != []
      aggregates? = query.aggregates != %{}
      limit? = not is_nil(query.limit)
      offset? = not (is_nil(query.offset) || query.offset == 0)
      filter? = not is_nil(query.filter)
      errors? = not Enum.empty?(query.errors)

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", inspect(query.resource)),
          or_empty(concat("filter: ", to_doc(query.filter, opts)), filter?),
          or_empty(concat("sort: ", to_doc(query.sort, opts)), sort?),
          or_empty(concat("limit: ", to_doc(query.limit, opts)), limit?),
          or_empty(concat("offset: ", to_doc(query.offset, opts)), offset?),
          or_empty(concat("side_load: ", to_doc(query.side_load, opts)), side_load?),
          or_empty(concat("aggregates: ", to_doc(query.aggregates, opts)), aggregates?),
          or_empty(concat("errors: ", to_doc(query.errors, opts)), errors?)
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end

    defp or_empty(value, true), do: value
    defp or_empty(_, false), do: empty()
  end

  alias Ash.Actions.Sort
  alias Ash.Error.Query.{AggregatesNotSupported, InvalidLimit, InvalidOffset}
  alias Ash.Error.SideLoad.{InvalidQuery, NoSuchRelationship}
  alias Ash.Query.Aggregate

  @doc "Create a new query."
  def new(resource, api \\ nil) when is_atom(resource) do
    %__MODULE__{
      api: api,
      filter: nil,
      resource: resource
    }
    |> set_data_layer_query()
  end

  @spec load(t(), atom | list(atom) | Keyword.t()) :: t()
  def load(query, fields) when not is_list(fields) do
    load(query, List.wrap(fields))
  end

  def load(query, fields) do
    query = to_query(query)

    Enum.reduce(fields, query, fn
      {field, %__MODULE__{} = nested}, query ->
        side_load(query, [{field, nested}])

      {field, rest}, query ->
        rel = Ash.Resource.relationship(query.resource, field)

        if rel do
          nested_query = load(rel.destination, rest)

          side_load(query, [{field, nested_query}])
        else
          add_error(query, :load, "Invalid load #{inspect(field)}")
        end

      field, query ->
        do_load(query, field)
    end)
  end

  defp do_load(query, field) do
    cond do
      Ash.Resource.attribute(query.resource, field) ->
        query

      Ash.Resource.relationship(query.resource, field) ->
        side_load(query, field)

      aggregate = Ash.Resource.aggregate(query.resource, field) ->
        with %{valid?: true} = aggregate_query <-
               build(query.resource, filter: aggregate.filter),
             {:ok, query_aggregate} <-
               Aggregate.new(
                 query.resource,
                 aggregate.name,
                 aggregate.kind,
                 aggregate.relationship_path,
                 aggregate_query
               ) do
          query_aggregate = %{query_aggregate | load: field}
          new_aggregates = Map.put(query.aggregates, aggregate.name, query_aggregate)

          %{query | aggregates: new_aggregates}
        else
          %{errors: errors} ->
            add_error(query, :aggregates, Ash.Error.to_ash_error(errors))

          {:error, error} ->
            add_error(query, :aggregates, Ash.Error.to_ash_error(error))
        end

      true ->
        add_error(query, :load, "Could not load #{inspect(field)}")
    end
  end

  @spec put_datalayer_context(t(), atom, term) :: t()
  def put_datalayer_context(query, key, value) do
    %{query | data_layer_context: Map.put(query.data_layer_context, key, value)}
  end

  @spec set_datalayer_context(t(), map) :: t()
  def set_datalayer_context(query, map) do
    %{query | data_layer_context: Map.merge(query.data_layer_context, map)}
  end

  def unload(query, fields) do
    query = to_query(query)

    Enum.reduce(fields, query, fn field, query ->
      case field do
        {field, rest} ->
          new_side_loads = do_unload_side_load(query.side_load, {field, rest})
          %{query | side_load: new_side_loads}

        field ->
          do_unload(query, field)
      end
    end)
  end

  defp do_unload(query, field) do
    cond do
      Ash.Resource.attribute(query.resource, field) ->
        query

      Ash.Resource.relationship(query.resource, field) ->
        %{query | side_load: Keyword.delete(query.side_load, field)}

      Ash.Resource.aggregate(query.resource, field) ->
        new_aggregates =
          Enum.reduce(query.aggregates, %{}, fn
            {_field, %{load: ^field}}, acc ->
              acc

            {field, aggregate}, acc ->
              Map.put(acc, field, aggregate)
          end)

        %{query | aggregates: new_aggregates}
    end
  end

  defp do_unload_side_load(%__MODULE__{} = query, unload) do
    %{query | side_load: do_unload_side_load(query.side_load, unload)}
  end

  defp do_unload_side_load(side_loads, {field, rest}) do
    Enum.reduce(side_loads, [], fn
      ^field, acc ->
        acc

      {^field, value}, acc ->
        new_value =
          rest
          |> List.wrap()
          |> Enum.reduce(value, &do_unload_side_load(&2, &1))

        [{field, new_value} | acc]

      value, acc ->
        [value | acc]
    end)
    |> Enum.reverse()
  end

  defp do_unload_side_load(side_loads, field) do
    do_unload_side_load(side_loads, {field, []})
  end

  @spec build(Ash.resource(), Ash.api() | nil, Keyword.t()) :: t()
  def build(resource, api \\ nil, keyword) do
    Enum.reduce(keyword, new(resource, api), fn
      {:filter, value}, query ->
        filter(query, value)

      {:sort, value}, query ->
        sort(query, value)

      {:limit, value}, query ->
        limit(query, value)

      {:offset, value}, query ->
        offset(query, value)

      {:side_load, value}, query ->
        side_load(query, value)

      {:aggregate, {name, type, relationship}}, query ->
        aggregate(query, name, type, relationship)

      {:aggregate, {name, type, relationship, agg_query}}, query ->
        aggregate(query, name, type, relationship, agg_query)
    end)
  end

  @doc "Set the query's api, and any side loaded query's api"
  def set_api(query, api) do
    query = to_query(query)
    %{query | api: api, side_load: set_side_load_api(query.side_load, api)}
  end

  @doc """
  Adds an aggregation to the query.

  Aggregations are made available on the `aggregates` field of the records returned

  The only aggregate available currently is a `count` aggregate. They filter option accepts
  either a filter or a keyword list of options to supply to build a limiting query for that aggregate.
  However, currently only filters are accepted.
  """
  @spec aggregate(
          t() | Ash.resource(),
          atom(),
          Ash.aggregate_kind(),
          atom | list(atom),
          Ash.query() | nil
        ) :: t()
  def aggregate(query, name, type, relationship, agg_query \\ nil) do
    query = to_query(query)
    relationship = List.wrap(relationship)

    if Ash.Resource.data_layer_can?(query.resource, {:aggregate, type}) do
      agg_query =
        case agg_query do
          nil ->
            nil

          %__MODULE__{} = agg_query ->
            agg_query

          options when is_list(options) ->
            build(Ash.Resource.related(query.resource, relationship), options)
        end

      case Aggregate.new(query.resource, name, type, relationship, agg_query) do
        {:ok, aggregate} ->
          new_aggregates = Map.put(query.aggregates, aggregate.name, aggregate)

          set_data_layer_query(%{query | aggregates: new_aggregates})

        {:error, error} ->
          add_error(query, :aggregate, error)
      end
    else
      add_error(
        query,
        :aggregate,
        AggregatesNotSupported.exception(resource: query.resource, feature: "using")
      )
    end
  end

  @doc "Limit the results returned from the query"
  @spec limit(t() | Ash.resource(), nil | integer()) :: t()
  def limit(query, nil), do: to_query(query)

  def limit(query, limit) when is_integer(limit) do
    query = to_query(query)

    if Ash.Resource.data_layer_can?(query.resource, :limit) do
      query
      |> Map.put(:limit, max(0, limit))
      |> set_data_layer_query()
    else
      add_error(query, :limit, "Data layer does not support limits")
    end
  end

  def limit(query, limit) do
    add_error(query, :offset, InvalidLimit.exception(limit: limit))
  end

  @doc "Skip the first n records"
  @spec offset(t() | Ash.resource(), nil | integer()) :: t()
  def offset(query, nil), do: to_query(query)

  def offset(query, offset) when is_integer(offset) do
    query = to_query(query)

    if Ash.Resource.data_layer_can?(query.resource, :offset) do
      query
      |> Map.put(:offset, max(0, offset))
      |> set_data_layer_query()
    else
      add_error(query, :offset, "Data layer does not support offset")
    end
  end

  def offset(query, offset) do
    query
    |> to_query()
    |> add_error(:offset, InvalidOffset.exception(offset: offset))
  end

  @doc "Side loads related entities"
  @spec side_load(t() | Ash.resource(), Ash.side_loads()) :: t()
  def side_load(query, statement) do
    query = to_query(query)

    with sanitized_statement <- List.wrap(sanitize_side_loads(statement)),
         :ok <- validate_side_load(query.resource, sanitized_statement),
         new_side_loads <- merge_side_load(query.side_load, sanitized_statement) do
      %{query | side_load: new_side_loads}
    else
      {:error, errors} ->
        Enum.reduce(errors, query, &add_error(&2, :side_load, &1))
    end
  end

  def validate_side_load(resource, side_loads, path \\ []) do
    case do_validate_side_load(resource, side_loads, path) do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  def do_validate_side_load(_resource, %Ash.Query{} = query, path) do
    if query.limit || (query.offset && query.offset != 0) do
      [{:error, InvalidQuery.exception(query: query, side_load_path: Enum.reverse(path))}]
    else
      case query.errors do
        [] ->
          []

        _errors ->
          [
            {:error,
             InvalidQuery.exception(
               query: query,
               side_load_path: Enum.reverse(path)
             )}
          ]
      end
    end
  end

  def do_validate_side_load(resource, {atom, _} = tuple, path) when is_atom(atom) do
    do_validate_side_load(resource, [tuple], path)
  end

  def do_validate_side_load(resource, side_loads, path) when is_list(side_loads) do
    side_loads
    |> List.wrap()
    |> Enum.flat_map(fn
      {_key, %Ash.Query{}} ->
        []

      {key, value} ->
        case Ash.Resource.relationship(resource, key) do
          nil ->
            [
              {:error,
               NoSuchRelationship.exception(
                 resource: resource,
                 relationship: key,
                 side_load_path: Enum.reverse(path)
               )}
            ]

          relationship ->
            validate_matching_query_and_continue(value, resource, key, path, relationship)
        end
    end)
  end

  @spec filter(t() | Ash.resource(), nil | false | Ash.filter() | Keyword.t()) :: t()
  def filter(query, nil), do: to_query(query)

  def filter(query, %Ash.Filter{} = filter) do
    query = to_query(query)

    if Ash.Resource.data_layer_can?(query.resource, :filter) do
      new_filter =
        case query.filter do
          nil ->
            {:ok, filter}

          existing_filter ->
            Ash.Filter.add_to_filter(existing_filter, filter, :and, query.aggregates)
        end

      case new_filter do
        {:ok, filter} ->
          set_data_layer_query(%{query | filter: filter})

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  def filter(query, statement) do
    query = to_query(query)

    if Ash.Resource.data_layer_can?(query.resource, :filter) do
      filter =
        if query.filter do
          Ash.Filter.add_to_filter(query.filter, statement, :and, query.aggregates)
        else
          Ash.Filter.parse(query.resource, statement, query.aggregates)
        end

      case filter do
        {:ok, filter} ->
          query
          |> Map.put(:filter, filter)
          |> set_data_layer_query()

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  @spec sort(t() | Ash.resource(), Ash.sort()) :: t()
  def sort(query, sorts) do
    query = to_query(query)

    if Ash.Resource.data_layer_can?(query.resource, :sort) do
      sorts
      |> List.wrap()
      |> Enum.reduce(query, fn
        {sort, direction}, query ->
          %{query | sort: query.sort ++ [{sort, direction}]}

        sort, query ->
          %{query | sort: query.sort ++ [{sort, :asc}]}
      end)
      |> validate_sort()
      |> set_data_layer_query()
    else
      add_error(query, :sort, "Data layer does not support sorting")
    end
  end

  @spec unset(Ash.resource() | t(), atom | [atom]) :: t()
  def unset(query, keys) when is_list(keys) do
    query = to_query(query)

    keys
    |> Enum.reduce(query, fn key, query ->
      if key in [:api, :resource] do
        query
      else
        struct(query, [{key, Map.get(%__MODULE__{}, key)}])
      end
    end)
    |> set_data_layer_query()
  end

  def unset(query, key) do
    if key in [:api, :resource] do
      to_query(query)
    else
      query
      |> to_query()
      |> struct([{key, Map.get(%__MODULE__{}, key)}])
      |> set_data_layer_query()
    end
  end

  @doc false
  def data_layer_query(%{resource: resource} = ash_query, opts \\ []) do
    if Ash.Resource.data_layer_can?(resource, :read) do
      query = Ash.DataLayer.resource_to_query(resource)

      filter_aggregates =
        if ash_query.filter do
          Ash.Filter.used_aggregates(ash_query.filter)
        else
          []
        end

      sort_aggregates =
        Enum.flat_map(ash_query.sort, fn {field, _} ->
          case Map.fetch(ash_query.aggregates, field) do
            :error ->
              []

            {:ok, agg} ->
              [agg]
          end
        end)

      aggregates = Enum.uniq_by(filter_aggregates ++ sort_aggregates, & &1.name)

      with {:ok, query} <-
             add_aggregates(query, ash_query.resource, aggregates),
           {:ok, query} <-
             Ash.DataLayer.sort(query, ash_query.sort, resource),
           {:ok, query} <- maybe_filter(query, ash_query, opts),
           {:ok, query} <-
             Ash.DataLayer.limit(query, ash_query.limit, resource),
           {:ok, query} <-
             Ash.DataLayer.offset(query, ash_query.offset, resource) do
        {:ok, Ash.DataLayer.set_context(resource, query, ash_query.data_layer_context)}
      else
        {:error, error} -> {:error, error}
      end
    else
      {:error, "Resource does not support reading"}
    end
  end

  defp add_aggregates(query, resource, aggregates) do
    Enum.reduce_while(aggregates, {:ok, query}, fn aggregate, {:ok, query} ->
      case Ash.DataLayer.add_aggregate(query, aggregate, resource) do
        {:ok, query} -> {:cont, {:ok, query}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp validate_sort(%{resource: resource, sort: sort} = query) do
    case Sort.process(resource, sort, query.aggregates) do
      {:ok, new_sort} -> %{query | sort: new_sort}
      {:error, error} -> add_error(query, :sort, error)
    end
  end

  defp add_error(query, key, message) do
    query = to_query(query)

    message =
      if is_binary(message) do
        "#{key}: #{message}"
      else
        message
      end

    %{
      query
      | errors: [Map.put(Ash.Error.to_ash_error(message), :path, key) | query.errors],
        valid?: false
    }
  end

  defp set_data_layer_query(query) do
    case data_layer_query(query) do
      {:ok, data_layer_query} -> %{query | data_layer_query: data_layer_query}
      {:error, error} -> add_error(query, :data_layer_query, error)
    end
  end

  defp validate_matching_query_and_continue(value, resource, key, path, relationship) do
    %{destination: relationship_resource} = relationship

    case value do
      %__MODULE__{resource: query_resource} = destination_query
      when query_resource != relationship_resource ->
        [
          InvalidQuery.exception(
            resource: resource,
            relationship: key,
            query: destination_query,
            side_load_path: Enum.reverse(path)
          )
        ]

      other ->
        do_validate_side_load(relationship.destination, other, [key | path])
    end
  end

  defp maybe_filter(query, %{filter: nil}, _) do
    {:ok, query}
  end

  defp maybe_filter(query, ash_query, opts) do
    case Ash.DataLayer.filter(query, ash_query.filter, ash_query.resource) do
      {:ok, filtered} ->
        if Keyword.get(opts, :only_validate_filter?, true) do
          {:ok, query}
        else
          {:ok, filtered}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp set_side_load_api(nil, _), do: nil
  defp set_side_load_api([], _), do: []

  defp set_side_load_api(%__MODULE__{} = query, api) do
    set_api(query, api)
  end

  defp set_side_load_api(side_loads, api) do
    Enum.map(side_loads, fn {key, further} ->
      {key, set_side_load_api(further, api)}
    end)
  end

  defp to_query(%__MODULE__{} = query), do: query

  defp to_query(resource) do
    resource
    |> new()
    |> Ash.DataLayer.transform_query()
  end

  defp merge_side_load([], right), do: sanitize_side_loads(right)
  defp merge_side_load(left, []), do: sanitize_side_loads(left)

  defp merge_side_load(
         %__MODULE__{side_load: left_side_loads},
         %__MODULE__{side_load: right_side_loads} = query
       ) do
    %{query | side_load: merge_side_load(left_side_loads, right_side_loads)}
  end

  defp merge_side_load(%__MODULE__{} = query, right) when is_list(right) do
    Ash.Query.side_load(query, right)
  end

  defp merge_side_load(left, %Ash.Query{} = query) when is_list(left) do
    Ash.Query.side_load(query, left)
  end

  defp merge_side_load(left, right) when is_atom(left), do: merge_side_load([{left, []}], right)
  defp merge_side_load(left, right) when is_atom(right), do: merge_side_load(left, [{right, []}])

  defp merge_side_load(left, right) when is_list(left) and is_list(right) do
    right
    |> sanitize_side_loads()
    |> Enum.reduce(sanitize_side_loads(left), fn {rel, rest}, acc ->
      Keyword.update(acc, rel, rest, &merge_side_load(&1, rest))
    end)
  end

  defp sanitize_side_loads(side_load) when is_atom(side_load), do: {side_load, []}

  defp sanitize_side_loads(%Ash.Query{} = query) do
    Map.update!(query, :side_load, &sanitize_side_loads/1)
  end

  defp sanitize_side_loads(side_loads) do
    side_loads
    |> List.wrap()
    |> Enum.map(fn
      {key, value} ->
        {key, sanitize_side_loads(value)}

      side_load_part ->
        cond do
          is_atom(side_load_part) -> {side_load_part, []}
          is_list(side_load_part) -> sanitize_side_loads(side_load_part)
          true -> side_load_part
        end
    end)
  end
end
