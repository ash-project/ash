defmodule Ash.Query do
  @moduledoc """
  Utilties around constructing/manipulating ash queries.

  Ash queries are used for read actions and side loads, and ultimately
  map to queries to a resource's data layer.

  Queries are run by calling `read` on an API that contains the resource in question

  Examples:

  ```elixir
  MyApp.Post
  |> Query.filter(likes > 10)
  |> Query.sort([:title])
  |> MyApp.Api.read!()

  MyApp.Author
  |> Query.aggregate(:published_post_count, :posts, filter: [published: true])
  |> Query.sort(published_post_count: :desc)
  |> Query.limit(10)
  |> MyApp.Api.read!()

  MyApp.Author
  |> Query.load([:post_count, :comment_count])
  |> Query.load(posts: [:comments])
  |> MyApp.Api.read!()
  ```
  """
  defstruct [
    :api,
    :resource,
    :filter,
    :tenant,
    aggregates: %{},
    side_load: [],
    calculations: %{},
    context: %{},
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
      calculations? = query.calculations != %{}
      limit? = not is_nil(query.limit)
      offset? = not (is_nil(query.offset) || query.offset == 0)
      filter? = not is_nil(query.filter)
      errors? = not Enum.empty?(query.errors)
      tenant? = not is_nil(query.tenant)

      container_doc(
        "#Ash.Query<",
        [
          concat("resource: ", inspect(query.resource)),
          or_empty(concat("tenant: ", to_doc(query.tenant, opts)), tenant?),
          or_empty(concat("filter: ", to_doc(query.filter, opts)), filter?),
          or_empty(concat("sort: ", to_doc(query.sort, opts)), sort?),
          or_empty(concat("limit: ", to_doc(query.limit, opts)), limit?),
          or_empty(concat("offset: ", to_doc(query.offset, opts)), offset?),
          or_empty(concat("side_load: ", to_doc(query.side_load, opts)), side_load?),
          or_empty(concat("aggregates: ", to_doc(query.aggregates, opts)), aggregates?),
          or_empty(concat("calculations: ", to_doc(query.calculations, opts)), calculations?),
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
  alias Ash.Query.{Aggregate, Calculation}

  @doc """
  Attach a filter statement to the query.

  The filter is applied as an "and" to any filters currently on the query.
  For more information on writing filters, see: `Ash.Filter`.
  """
  defmacro filter(query, %Ash.Filter{} = filter) do
    quote do
      Ash.Query.do_filter(unquote(query), unquote(filter))
    end
  end

  defmacro filter(query, nil), do: query
  defmacro filter(query, true), do: query

  defmacro filter(query, false) do
    quote do
      Ash.Query.do_filter(unquote(query), false)
    end
  end

  defmacro filter(query, do: body) do
    quote do
      Ash.Query.do_filter(unquote(query), unquote(body))
    end
  end

  defmacro filter(query, expression) do
    if Keyword.keyword?(expression) do
      quote do
        Ash.Query.do_filter(unquote(query), unquote(expression))
      end
    else
      expr = do_expr(expression)

      quote do
        Ash.Query.do_filter(unquote(query), List.wrap(unquote(expr)))
      end
    end
  end

  @doc "Create a new query"
  def new(resource, api \\ nil)
  def new(%__MODULE__{} = query, _), do: query

  def new(resource, api) when is_atom(resource) do
    query = %__MODULE__{
      api: api,
      filter: nil,
      resource: resource
    }

    case Ash.Resource.base_filter(resource) do
      nil ->
        query

      filter ->
        filter = Ash.Filter.parse!(resource, filter)

        filter =
          Ash.Filter.map(filter, fn
            %{__predicate__?: true} = pred ->
              %{pred | embedded?: true}

            other ->
              other
          end)

        do_filter(query, filter)
    end
  end

  defmacro expr(do: body) do
    quote do
      Ash.Query.expr(unquote(body))
    end
  end

  defmacro expr({var, _, context} = binding) when is_atom(var) and is_atom(context) do
    quote do
      unquote(binding)
    end
  end

  defmacro expr(body) do
    if Keyword.keyword?(body) do
      quote do
        unquote(body)
      end
    else
      quote do
        List.wrap(unquote(do_expr(body)))
      end
    end
  end

  defp do_expr({:^, _, [var]}), do: var

  defp do_expr({:., _, [left, right]} = ref) when is_atom(right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{} = ref ->
        Macro.escape(ref)

      :error ->
        raise "Invalid reference! #{Macro.to_string(ref)}"
    end
  end

  defp do_expr({op, _, nil}) when is_atom(op) do
    Macro.escape(%Ash.Query.Ref{relationship_path: [], attribute: op})
  end

  defp do_expr({op, _, args}) when is_atom(op) and is_list(args) do
    {op, Enum.map(args, &do_expr(&1))}
  end

  defp do_expr({left, _, _}) when is_tuple(left), do: do_expr(left)

  defp do_expr(other), do: other

  defp do_ref({_, _, list}, _right) when is_list(list) do
    :error
  end

  defp do_ref({left, _, _}, right) when is_atom(left) and is_atom(right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref({:., _, [left, right]}, far_right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{relationship_path: path, attribute: attribute} = ref ->
        %{ref | relationship_path: path ++ [attribute], attribute: far_right}

      :error ->
        :error
    end
  end

  defp do_ref(_left, _right) do
    :error
  end

  @doc """
  Loads named calculations or aggregates on the resource.

  Currently, loading attributes has no effects, as all attributes are returned.
  Before long, we will have the default list to load as the attributes, but if you say
  `load(query, [:attribute1])`, that will be the only field filled in. This will let
  data layers make more intelligent "select" statements as well.
  """
  @spec load(t() | Ash.resource(), atom | list(atom) | Keyword.t()) :: t()
  def load(query, fields) when not is_list(fields) do
    load(query, List.wrap(fields))
  end

  def load(query, fields) do
    query = to_query(query)

    Enum.reduce(fields, query, fn
      {field, %__MODULE__{} = nested}, query ->
        side_load(query, [{field, nested}])

      {field, rest}, query ->
        cond do
          rel = Ash.Resource.relationship(query.resource, field) ->
            nested_query = load(rel.destination, rest)

            side_load(query, [{field, nested_query}])

          calculation = Ash.Resource.calculation(query.resource, field) ->
            {module, opts} = module_and_opts(calculation.calculation)

            with {:ok, args} <- validate_arguments(calculation, rest),
                 {:ok, calculation} <-
                   Calculation.new(
                     calculation.name,
                     module,
                     opts,
                     args
                   ) do
              calculation = %{calculation | load: field}
              %{query | calculations: Map.put(query.calculations, field, calculation)}
            end

          true ->
            add_error(query, :load, "Invalid load #{inspect(field)}")
        end

      field, query ->
        do_load(query, field)
    end)
  end

  defp module_and_opts({module, opts}), do: {module, opts}
  defp module_and_opts(module), do: {module, []}

  defp do_load(query, field) do
    cond do
      Ash.Resource.attribute(query.resource, field) ->
        query

      Ash.Resource.relationship(query.resource, field) ->
        side_load(query, field)

      aggregate = Ash.Resource.aggregate(query.resource, field) ->
        related = Ash.Resource.related(query.resource, aggregate.relationship_path)

        with %{valid?: true} = aggregate_query <-
               build(related, filter: aggregate.filter),
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

      calculation = Ash.Resource.calculation(query.resource, field) ->
        {module, opts} =
          case calculation.calculation do
            {module, opts} -> {module, opts}
            module -> {module, []}
          end

        with {:ok, args} <- validate_arguments(calculation, %{}),
             {:ok, calculation} <-
               Calculation.new(calculation.name, module, opts, args) do
          calculation = %{calculation | load: field}
          %{query | calculations: Map.put(query.calculations, field, calculation)}
        else
          {:error, error} ->
            add_error(query, :load, error)
        end

      true ->
        add_error(query, :load, "Could not load #{inspect(field)}")
    end
  end

  defp validate_arguments(calculation, args) do
    Enum.reduce_while(calculation.arguments, {:ok, %{}}, fn argument, {:ok, arg_values} ->
      value = default(Map.get(args, argument.name), argument.default)

      if is_nil(value) do
        if argument.allow_nil? do
          {:cont, {:ok, Map.put(arg_values, argument.name, nil)}}
        else
          {:halt, {:error, "Argument #{argument.name} is required"}}
        end
      else
        with {:ok, casted} <- Ash.Type.cast_input(argument.type, value),
             :ok <-
               Ash.Type.apply_constraints(argument.type, casted, argument.constraints) do
          {:cont, {:ok, Map.put(arg_values, argument.name, casted)}}
        else
          {:error, error} ->
            {:halt, {:error, error}}
        end
      end
    end)
  end

  defp default(nil, {module, function, args}), do: apply(module, function, args)
  defp default(nil, value) when is_function(value, 0), do: value.()
  defp default(nil, value), do: value
  defp default(value, _), do: value

  @doc """
  Sets a specific context key to a specific value

  See `set_context/2` for more information.
  """
  @spec put_context(t(), atom, term) :: t()
  def put_context(query, key, value) do
    %{query | context: Map.put(query.context, key, value)}
  end

  @doc """
  Merge a map of values into the query context

  Not much uses this currently.
  """
  @spec set_context(t(), map) :: t()
  def set_context(query, map) do
    %{
      query
      | context:
          Map.merge(query.context, map, fn _k, v1, v2 ->
            if is_map(v1) and is_map(v2) and not (is_struct(v1) || is_struct(v2)) do
              Map.merge(v1, v2)
            else
              v2
            end
          end)
    }
  end

  @spec set_tenant(t() | Ash.resource(), String.t()) :: t()
  def set_tenant(query, tenant) do
    query = to_query(query)
    %{query | tenant: tenant}
  end

  @doc "Removes a field from the list of fields to load"
  @spec unload(t(), list(atom)) :: t()
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

  @doc """
  Builds a query from a keyword list.

  This is used by certain query constructs like aggregates. It can also be used to manipulate a data structure
  before passing it to an ash query. It allows for building an entire query struct using only a keyword list.

  For example:

  ```elixir
  Ash.Query.build(MyResource, filter: [name: "fred"], sort: [name: :asc], load: [:foo, :bar], offset: 10)
  ```

  If you want to use the expression style filters, you can use `expr/1`. Be sure to `require Ash.Query` first,
  or import it. Consider importing only the `expr/1` macro if you do that

  For example:

  ```elixir
  import Ash.Query, only: [expr: 1]

  Ash.Query.build(Myresource, filter: expr(name == "marge"))
  ```
  """
  @spec build(Ash.resource(), Ash.api() | nil, Keyword.t()) :: t()
  def build(resource, api \\ nil, keyword) do
    Enum.reduce(keyword, new(resource, api), fn
      {:filter, value}, query ->
        do_filter(query, value)

      {:sort, value}, query ->
        sort(query, value)

      {:limit, value}, query ->
        limit(query, value)

      {:offset, value}, query ->
        offset(query, value)

      {:load, value}, query ->
        load(query, value)

      {:aggregate, {name, type, relationship}}, query ->
        aggregate(query, name, type, relationship)

      {:aggregate, {name, type, relationship, agg_query}}, query ->
        aggregate(query, name, type, relationship, agg_query)

      {:calculate, {name, module_and_opts}}, query ->
        calculate(query, name, module_and_opts)

      {:calculate, {name, module_and_opts, context}}, query ->
        calculate(query, name, module_and_opts, context)
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

          %{query | aggregates: new_aggregates}

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

  @doc """
  Adds a calculation to the query.

  Calculations are made available on the `calculations` field of the records returned

  The `module_and_opts` argument accepts either a `module` or a `{module, opts}`. For more information
  on what that module should look like, see `Ash.Calculation`.

  More features for calculations, like passing anonymous functions, will be supported in the future.
  """
  def calculate(query, name, module_and_opts, context \\ %{}) do
    query = to_query(query)

    {module, opts} =
      case module_and_opts do
        {module, opts} -> {module, opts}
        module -> {module, []}
      end

    case Calculation.new(name, module, opts, context) do
      {:ok, calculation} ->
        %{query | calculations: Map.put(query.calculations, name, calculation)}

      {:error, error} ->
        add_error(query, :calculations, error)
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
    else
      add_error(query, :offset, "Data layer does not support offset")
    end
  end

  def offset(query, offset) do
    query
    |> to_query()
    |> add_error(:offset, InvalidOffset.exception(offset: offset))
  end

  defp side_load(query, statement) do
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

  @doc false
  def validate_side_load(resource, side_loads, path \\ []) do
    case do_validate_side_load(resource, side_loads, path) do
      [] -> :ok
      errors -> {:error, errors}
    end
  end

  defp do_validate_side_load(_resource, %Ash.Query{} = query, path) do
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

  defp do_validate_side_load(resource, {atom, _} = tuple, path) when is_atom(atom) do
    do_validate_side_load(resource, [tuple], path)
  end

  defp do_validate_side_load(resource, side_loads, path) when is_list(side_loads) do
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

  @doc false
  def do_filter(query, %Ash.Filter{} = filter) do
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
          %{query | filter: filter}

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  def do_filter(query, statement) do
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

        {:error, error} ->
          add_error(query, :filter, error)
      end
    else
      add_error(query, :filter, "Data layer does not support filtering")
    end
  end

  @doc """
  Sort the results based on attributes or aggregates (calculations are not yet supported)

  Takes a list of fields to sort on, or a keyword list/mixed keyword list of fields and sort directions.
  The default sort direction is `:asc`.

  Examples:

  ```
  Ash.Query.sort(query, [:foo, :bar])

  Ash.Query.sort(query, [:foo, bar: :desc])

  Ash.Query.sort(query, [foo: :desc, bar: :asc])
  ```
  """
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
    else
      add_error(query, :sort, "Data layer does not support sorting")
    end
  end

  @doc """
  Reverse the sort order of a query.

  If the query has no sort, an error is added indicating that.
  """
  @spec reverse(t()) :: t()
  def reverse(%{sort: nil} = query) do
    add_error(query, :sort, "Unreversable sort")
  end

  def reverse(query) do
    query
    |> Ash.Query.unset(:sort)
    |> Ash.Query.sort(Ash.Actions.Sort.reverse(query.sort))
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
  end

  def unset(query, key) do
    if key in [:api, :resource] do
      to_query(query)
    else
      query
      |> to_query()
      |> struct([{key, Map.get(%__MODULE__{}, key)}])
    end
  end

  @doc "Return the underlying data layer query for an ash query"
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
             add_aggregates(query, ash_query, aggregates),
           {:ok, query} <-
             Ash.DataLayer.sort(query, ash_query.sort, resource),
           {:ok, query} <- maybe_filter(query, ash_query, opts),
           {:ok, query} <- add_tenant(query, ash_query),
           {:ok, query} <-
             Ash.DataLayer.limit(query, ash_query.limit, resource),
           {:ok, query} <-
             Ash.DataLayer.offset(query, ash_query.offset, resource) do
        {:ok, Ash.DataLayer.set_context(resource, query, ash_query.context)}
      else
        {:error, error} -> {:error, error}
      end
    else
      {:error, "Resource does not support reading"}
    end
  end

  defp add_tenant(query, ash_query) do
    with :context <- Ash.Resource.multitenancy_strategy(ash_query.resource),
         tenant when not is_nil(tenant) <- ash_query.tenant,
         {:ok, query} <- Ash.DataLayer.set_tenant(ash_query.resource, query, tenant) do
      {:ok, query}
    else
      {:error, error} -> {:error, error}
      _ -> {:ok, query}
    end
  end

  defp add_aggregates(query, ash_query, aggregates) do
    resource = ash_query.resource

    aggregates
    |> Enum.map(&add_tenant_to_aggregate_query(&1, ash_query))
    |> Enum.reduce_while({:ok, query}, fn aggregate, {:ok, query} ->
      case Ash.DataLayer.add_aggregate(query, aggregate, resource) do
        {:ok, query} -> {:cont, {:ok, query}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp add_tenant_to_aggregate_query(aggregate, %{tenant: nil}), do: aggregate

  defp add_tenant_to_aggregate_query(%{query: nil} = aggregate, ash_query) do
    aggregate_with_query = %{aggregate | query: Ash.Query.new(aggregate.resource)}
    add_tenant_to_aggregate_query(aggregate_with_query, ash_query)
  end

  defp add_tenant_to_aggregate_query(aggregate, ash_query) do
    case Ash.Resource.multitenancy_strategy(aggregate.resource) do
      nil ->
        aggregate

      :attribute ->
        attribute = Ash.Resource.multitenancy_attribute(aggregate.resource)
        {m, f, a} = Ash.Resource.multitenancy_parse_attribute(ash_query.resource)
        attribute_value = apply(m, f, [ash_query.tenant | a])
        %{aggregate | query: filter(aggregate.query, ^[{attribute, attribute_value}])}

      :context ->
        %{aggregate | query: set_tenant(aggregate.query, ash_query.tenant)}
    end
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
         %__MODULE__{side_load: left_side_loads, tenant: left_tenant},
         %__MODULE__{side_load: right_side_loads} = query
       ) do
    %{query | side_load: merge_side_load(left_side_loads, right_side_loads)}
    |> set_tenant(query.tenant || left_tenant)
  end

  defp merge_side_load(%__MODULE__{} = query, right) when is_list(right) do
    side_load(query, right)
  end

  defp merge_side_load(left, %Ash.Query{} = query) when is_list(left) do
    side_load(query, left)
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
