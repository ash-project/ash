defmodule Ash.DataLayer do
  @moduledoc """
  The interface for being an ash data layer.

  This is a large behaviour, and this capability is not complete, but the idea
  is to have a large amount of optional callbacks, and use the `can?/2` callback
  to ensure that the engine only ever tries to interact with the data layer in ways
  that it supports.
  """
  @type feature() ::
          :transact
          | {:lateral_join, Ash.resource()}
          | {:join, Ash.resource()}
          | {:aggregate, Ash.aggregate_kind()}
          | {:query_aggregate, Ash.aggregate_kind()}
          | :aggregate_filter
          | :aggregate_sort
          | :boolean_filter
          | :async_engine
          | :create
          | :read
          | :update
          | :destroy
          | :limit
          | :offset
          | :transact
          | :filter
          | {:filter_operator, struct}
          | {:filter_predicate, Ash.Type.t(), struct}
          | :sort
          | {:sort, Ash.Type.t()}
          | :upsert
          | :composite_primary_key

  @callback functions(Ash.resource()) :: [module]
  @callback operators(Ash.resource()) :: [module]
  @callback filter(Ash.data_layer_query(), Ash.filter(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, term}
  @callback sort(Ash.data_layer_query(), Ash.sort(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, term}
  @callback limit(Ash.data_layer_query(), limit :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, term}
  @callback offset(
              Ash.data_layer_query(),
              offset :: non_neg_integer(),
              resource :: Ash.resource()
            ) :: {:ok, Ash.data_layer_query()} | {:error, term}
  @callback resource_to_query(Ash.resource()) :: Ash.data_layer_query()
  @callback transform_query(Ash.query()) :: Ash.query()
  @callback run_query(Ash.data_layer_query(), Ash.resource()) ::
              {:ok, list(Ash.resource())} | {:error, term}
  @callback equal?(Ash.data_layer()) :: boolean
  @callback run_aggregate_query(Ash.data_layer_query(), list(Ash.aggregate()), Ash.resource()) ::
              {:ok, map} | {:error, term}
  @callback run_aggregate_query_with_lateral_join(
              Ash.data_layer_query(),
              list(Ash.aggregate()),
              [Ash.record()],
              source_resource :: Ash.resource(),
              destination_resource :: Ash.resource(),
              source :: atom,
              destination :: atom
            ) ::
              {:ok, list(Ash.resource())} | {:error, term}
  @callback run_query_with_lateral_join(
              Ash.data_layer_query(),
              [Ash.record()],
              source_resource :: Ash.resource(),
              destination_resource :: Ash.resource(),
              source :: atom,
              destination :: atom
            ) ::
              {:ok, list(Ash.resource())} | {:error, term}
  @callback create(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback upsert(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback update(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback add_aggregate(Ash.data_layer_query(), Ash.aggregate(), Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, term}
  @callback destroy(Ash.resource(), Ash.changeset()) :: :ok | {:error, term}
  @callback transaction(Ash.resource(), (() -> term)) :: {:ok, term} | {:error, term}
  @callback in_transaction?(Ash.resource()) :: boolean
  @callback source(Ash.resource()) :: String.t()
  @callback rollback(Ash.resource(), term) :: no_return
  @callback can?(Ash.resource(), feature()) :: boolean
  @callback set_context(Ash.resource(), Ash.data_layer_query(), map) :: Ash.data_layer_query()

  @optional_callbacks source: 1,
                      equal?: 1,
                      run_query: 2,
                      run_query_with_lateral_join: 6,
                      create: 2,
                      update: 2,
                      set_context: 3,
                      destroy: 2,
                      filter: 3,
                      sort: 3,
                      limit: 3,
                      offset: 3,
                      transaction: 2,
                      rollback: 2,
                      upsert: 2,
                      operators: 1,
                      functions: 1,
                      in_transaction?: 1,
                      add_aggregate: 3,
                      run_aggregate_query: 3,
                      run_aggregate_query_with_lateral_join: 7,
                      transform_query: 1,
                      resource_to_query: 1

  @spec resource_to_query(Ash.resource()) :: Ash.data_layer_query()
  def resource_to_query(resource) do
    Ash.Resource.data_layer(resource).resource_to_query(resource)
  end

  @spec update(Ash.resource(), Ash.changeset()) ::
          {:ok, Ash.record()} | {:error, term}
  def update(resource, changeset) do
    Ash.Resource.data_layer(resource).update(resource, changeset)
  end

  @spec create(Ash.resource(), Ash.changeset()) ::
          {:ok, Ash.record()} | {:error, term}
  def create(resource, changeset) do
    Ash.Resource.data_layer(resource).create(resource, changeset)
  end

  @spec destroy(Ash.resource(), Ash.changeset()) :: :ok | {:error, term}
  def destroy(resource, changeset) do
    Ash.Resource.data_layer(resource).destroy(resource, changeset)
  end

  @spec source(Ash.resource()) :: String.t()
  def source(resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :source, 1) do
      data_layer.source(resource)
    else
      ""
    end
  end

  @spec upsert(Ash.resource(), Ash.changeset()) ::
          {:ok, Ash.record()} | {:error, term}
  def upsert(resource, changeset) do
    Ash.Resource.data_layer(resource).upsert(resource, changeset)
  end

  @spec set_context(Ash.resource(), Ash.data_layer_query(), map) :: Ash.data_layer_query()
  def set_context(resource, query, map) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :set_context, 2) do
      data_layer.set_context(query, map)
    else
      query
    end
  end

  @spec filter(Ash.data_layer_query(), Ash.filter(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def filter(query, nil, _), do: {:ok, query}

  def filter(query, filter, resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if data_layer.can?(resource, :filter) do
      if data_layer.can?(resource, :boolean_filter) do
        data_layer.filter(query, filter, resource)
      else
        simple_filter = Ash.Filter.to_simple_filter(filter)
        data_layer.filter(query, simple_filter, resource)
      end
    else
      {:error, "Data layer does not support filtering"}
    end
  end

  @spec sort(Ash.data_layer_query(), Ash.sort(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def sort(query, sort, resource) do
    if can?(:sort, resource) do
      data_layer = Ash.Resource.data_layer(resource)
      data_layer.sort(query, sort, resource)
    else
      {:ok, query}
    end
  end

  @spec limit(Ash.data_layer_query(), limit :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def limit(query, nil, _resource), do: {:ok, query}

  def limit(query, limit, resource) do
    if can?(:limit, resource) do
      data_layer = Ash.Resource.data_layer(resource)
      data_layer.limit(query, limit, resource)
    else
      {:ok, query}
    end
  end

  @spec offset(Ash.data_layer_query(), offset :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def offset(query, nil, _resource), do: {:ok, query}

  def offset(query, offset, resource) do
    if can?(:offset, resource) do
      data_layer = Ash.Resource.data_layer(resource)
      data_layer.offset(query, offset, resource)
    else
      {:ok, query}
    end
  end

  @spec add_aggregate(Ash.data_layer_query(), Ash.aggregate(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def add_aggregate(query, aggregate, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.add_aggregate(query, aggregate, resource)
  end

  @spec can?(feature, Ash.resource()) :: boolean
  def can?(feature, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.can?(resource, feature)
  end

  @spec run_aggregate_query(Ash.data_layer_query(), list(Ash.aggregate()), Ash.resource()) ::
          {:ok, map} | {:error, term}
  def run_aggregate_query(query, aggregates, resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :run_aggregate_query, 3) do
      data_layer.run_aggregate_query(query, aggregates, resource)
    else
      {:error, "Aggregate queries not supported"}
    end
  end

  @spec run_query(Ash.data_layer_query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, term}
  def run_query(query, central_resource) do
    Ash.Resource.data_layer(central_resource).run_query(query, central_resource)
  end

  def run_aggregate_query_with_lateral_join(
        query,
        aggregates,
        root_data,
        source_resource,
        destination_resource,
        source,
        destination
      ) do
    Ash.Resource.data_layer(source_resource).run_query_with_lateral_join(
      query,
      aggregates,
      root_data,
      source_resource,
      destination_resource,
      source,
      destination
    )
  end

  def run_query_with_lateral_join(
        query,
        root_data,
        source_resource,
        destination_resource,
        source,
        destination
      ) do
    Ash.Resource.data_layer(source_resource).run_query_with_lateral_join(
      query,
      root_data,
      source_resource,
      destination_resource,
      source,
      destination
    )
  end

  def transact(resource, func) do
    if can?(:transact, resource) do
      data_layer = Ash.Resource.data_layer(resource)
      data_layer.transaction(resource, func)
    else
      {:ok, func.()}
    end
  end

  def operators(resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :operators, 1) do
      data_layer.operators(resource)
    else
      %{}
    end
  end

  def functions(resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :functions, 1) do
      data_layer.functions(resource)
    else
      %{}
    end
  end

  def transform_query(query) do
    data_layer = Ash.Resource.data_layer(query.resource)

    if :erlang.function_exported(data_layer, :transform_query, 1) do
      data_layer.transform_query(query)
    else
      query
    end
  end
end
