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
          | :aggregate
          | :aggregate_filter
          | :aggregate_sort
          | :boolean_filter
          | :async_engine
          | :join
          | :transact
          | {:filter_predicate, Ash.Type.t(), struct}
          | {:sort, Ash.Type.t()}
          | :upsert
          | :delete_with_query
          | :composite_primary_key

  @callback custom_filters(Ash.resource()) :: map()
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
  @callback run_query(Ash.data_layer_query(), Ash.resource()) ::
              {:ok, list(Ash.resource())} | {:error, term}
  @callback create(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback upsert(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback update(Ash.resource(), Ash.changeset()) ::
              {:ok, Ash.resource()} | {:error, term}
  @callback add_aggregate(Ash.data_layer_query(), Ash.aggregate(), Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, term}
  @callback destroy(record :: Ash.record()) :: :ok | {:error, term}
  @callback transaction(Ash.resource(), (() -> term)) :: {:ok, term} | {:error, term}
  @callback in_transaction?(Ash.resource()) :: boolean
  @callback source(Ash.resource()) :: String.t()
  @callback rollback(Ash.resource(), term) :: no_return
  @callback can?(Ash.resource(), feature()) :: boolean

  @optional_callbacks source: 1,
                      filter: 3,
                      sort: 3,
                      limit: 3,
                      offset: 3,
                      transaction: 2,
                      rollback: 2,
                      upsert: 2,
                      custom_filters: 1,
                      in_transaction?: 1,
                      add_aggregate: 3

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

  @spec source(Ash.resource()) :: String.t()
  def source(resource) do
    if :erlang.function_exported(Ash.Resource.data_layer(resource), :source, 1) do
      Ash.Resource.data_layer(resource).source(resource)
    else
      ""
    end
  end

  @spec upsert(Ash.resource(), Ash.changeset()) ::
          {:ok, Ash.record()} | {:error, term}
  def upsert(resource, changeset) do
    Ash.Resource.data_layer(resource).upsert(resource, changeset)
  end

  @spec filter(Ash.data_layer_query(), Ash.filter(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def filter(query, nil, _), do: {:ok, query}

  def filter(query, filter, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.filter(query, filter, resource)
  end

  @spec sort(Ash.data_layer_query(), Ash.sort(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def sort(query, sort, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.sort(query, sort, resource)
  end

  @spec limit(Ash.data_layer_query(), limit :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def limit(query, nil, _resource), do: {:ok, query}

  def limit(query, limit, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.limit(query, limit, resource)
  end

  @spec offset(Ash.data_layer_query(), offset :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, term}
  def offset(query, nil, _resource), do: {:ok, query}

  def offset(query, offset, resource) do
    data_layer = Ash.Resource.data_layer(resource)
    data_layer.offset(query, offset, resource)
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

  @spec run_query(Ash.data_layer_query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, term}
  def run_query(query, central_resource) do
    Ash.Resource.data_layer(central_resource).run_query(query, central_resource)
  end

  def transact(resource, func) do
    if can?(:transact, resource) do
      data_layer = Ash.Resource.data_layer(resource)
      data_layer.transaction(resource, func)
    else
      {:ok, func.()}
    end
  end

  def custom_filters(resource) do
    data_layer = Ash.Resource.data_layer(resource)

    if :erlang.function_exported(data_layer, :custom_filters, 1) do
      data_layer.custom_filters(resource)
    else
      %{}
    end
  end
end
