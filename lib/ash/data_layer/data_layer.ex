defmodule Ash.DataLayer do
  @type filter_type :: :eq | :in
  @type feature() :: :transact | :query_async | {:filter, filter_type}

  @callback filter(Ash.data_layer_query(), Ash.filter(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  @callback sort(Ash.data_layer_query(), Ash.sort(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  @callback limit(Ash.data_layer_query(), limit :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  @callback offset(
              Ash.data_layer_query(),
              offset :: non_neg_integer(),
              resource :: Ash.resource()
            ) :: {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  @callback resource_to_query(Ash.resource()) :: Ash.data_layer_query()
  @callback can_query_async?(Ash.resource()) :: boolean
  @callback run_query(Ash.data_layer_query(), Ash.resource()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}
  @callback create(Ash.resource(), changeset :: Ecto.Changeset.t()) ::
              {:ok, Ash.resource()} | {:error, Ash.error()}
  @callback update(Ash.resource(), changeset :: Ecto.Changeset.t()) ::
              {:ok, Ash.resource()} | {:error, Ash.error()}
  @callback destroy(record :: Ash.record()) :: :ok | {:error, Ash.error()}
  @callback transaction(Ash.resource(), (() -> term)) :: {:ok, term} | {:error, Ash.error()}
  @callback can?(feature()) :: boolean

  @optional_callbacks transaction: 2

  @spec resource_to_query(Ash.resource()) :: Ash.data_layer_query()
  def resource_to_query(resource) do
    Ash.data_layer(resource).resource_to_query(resource)
  end

  @spec update(Ash.resource(), Ecto.Changeset.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def update(resource, changeset) do
    Ash.data_layer(resource).update(resource, changeset)
  end

  @spec create(Ash.resource(), Ecto.Changeset.t()) ::
          {:ok, Ash.record()} | {:error, Ash.error()}
  def create(resource, changeset) do
    Ash.data_layer(resource).create(resource, changeset)
  end

  @spec filter(Ash.data_layer_query(), Ash.filter(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  def filter(query, nil, _), do: {:ok, query}

  def filter(query, filter, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.filter(query, filter, resource)
  end

  @spec sort(Ash.data_layer_query(), Ash.sort(), Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  def sort(query, sort, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.sort(query, sort, resource)
  end

  @spec limit(Ash.data_layer_query(), limit :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  def limit(query, nil, _resource), do: {:ok, query}

  def limit(query, limit, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.limit(query, limit, resource)
  end

  @spec offset(Ash.data_layer_query(), offset :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.data_layer_query()} | {:error, Ash.error()}
  def offset(query, nil, _resource), do: {:ok, query}

  def offset(query, offset, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.offset(query, offset, resource)
  end

  @spec can?(feature, Ash.resource()) :: boolean
  def can?(feature, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.can?(feature)
  end

  @spec run_query(Ash.data_layer_query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, Ash.error()}
  def run_query(query, central_resource) do
    Ash.data_layer(central_resource).run_query(query, central_resource)
  end

  def transact(resource, func) do
    if can?(:transact, resource) do
      data_layer = Ash.data_layer(resource)
      data_layer.transaction(resource, func)
    else
      {:ok, func.()}
    end
  end
end
