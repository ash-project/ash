defmodule Ash.DataLayer do
  @type filter_type :: :eq | :in
  @type feature() :: :transact | :query_async | {:filter, filter_type}

  @callback filter(Ash.query(), Ash.filter(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback sort(Ash.query(), Ash.sort(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback limit(Ash.query(), limit :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback offset(Ash.query(), offset :: non_neg_integer(), resource :: Ash.resource()) ::
              {:ok, Ash.query()} | {:error, Ash.error()}
  @callback resource_to_query(Ash.resource()) :: Ash.query()
  @callback can_query_async?(Ash.resource()) :: boolean
  @callback run_query(Ash.query(), Ash.resource()) ::
              {:ok, list(Ash.resource())} | {:error, Ash.error()}
  @callback create(Ash.resource(), changeset :: Ecto.Changeset.t()) ::
              {:ok, Ash.resource()} | {:error, Ash.error()}
  @callback update(Ash.resource(), changeset :: Ecto.Changeset.t()) ::
              {:ok, Ash.resource()} | {:error, Ash.error()}
  @callback destroy(record :: Ash.record()) :: :ok | {:error, Ash.error()}
  @callback transaction((() -> term)) :: term
  @callback can?(feature()) :: boolean

  @optional_callbacks transaction: 1

  @spec resource_to_query(Ash.resource()) :: Ash.query()
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

  @spec filter(Ash.query(), Ash.filter(), Ash.resource()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  def filter(query, filter, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.filter(query, filter, resource)
  end

  @spec sort(Ash.query(), Ash.sort(), Ash.resource()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  def sort(query, sort, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.sort(query, sort, resource)
  end

  @spec limit(Ash.query(), limit :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  def limit(query, limit, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.limit(query, limit, resource)
  end

  @spec offset(Ash.query(), offset :: non_neg_integer, Ash.resource()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  def offset(query, offset, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.limit(query, offset, resource)
  end

  @spec can?(feature, Ash.resource()) :: boolean
  def can?(feature, resource) do
    data_layer = Ash.data_layer(resource)
    data_layer.can?(feature)
  end

  @spec run_query(Ash.query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, Ash.error()}
  def run_query(query, central_resource) do
    Ash.data_layer(central_resource).run_query(query, central_resource)
  end

  def transact(resource, func) do
    if can?(:transact, resource) do
      data_layer = Ash.data_layer(resource)
      data_layer.transaction(func)
    else
      func.()
    end
  end
end
