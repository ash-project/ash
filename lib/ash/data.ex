defmodule Ash.Data do
  @spec get_by_id(Ash.resource(), id :: any) :: {:ok, Ash.record() | nil} | {:error, Ash.error()}
  def get_by_id(resource, id) do
    data_layer = Ash.data_layer(resource)

    with {:ok, query} <- data_layer.resource_to_query(resource),
         {:ok, query} <- data_layer.filter(query, :id, id, resource) do
      data_layer.get_one(query, resource)
    end
  end

  @spec resource_to_query(Ash.resource()) :: Ash.query()
  def resource_to_query(resource) do
    Ash.data_layer(resource).resource_to_query(resource)
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

  @spec get_related(Ash.record(), Ash.relationship()) ::
          {:ok, list(Ash.record()) | Ash.record() | nil} | {:error, Ash.error()}
  def get_related(record, %{cardinality: :many} = relationship) do
    case relationship_query(record, relationship) do
      {:ok, query} ->
        get_many(query, Ash.to_resource(record))

      {:error, error} ->
        {:error, error}
    end
  end

  def get_related(record, %{cardinality: :one} = relationship) do
    case relationship_query(record, relationship) do
      {:ok, query} ->
        get_one(query, Ash.to_resource(record))

      {:error, error} ->
        {:error, error}
    end
  end

  # This is a very hamfisted approach. Later, this will need to be split up
  # into things like "get batch" and whatnot. This will happen when I integrate
  # dataloader
  @spec side_load(list(Ash.record()), Ash.side_load_keyword(), Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, Ash.error()}
  def side_load(records, side_load_keyword, resource) do
    Ash.data_layer(resource).side_load(records, side_load_keyword, resource)
  end

  @spec get_one(Ash.query(), central_resource :: Ash.resource()) ::
          {:ok, Ash.record() | nil} | {:error, Ash.error()}
  def get_one(query, central_resource) do
    Ash.data_layer(central_resource).get_one(query, central_resource)
  end

  @spec get_many(Ash.query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, Ash.error()}
  def get_many(query, central_resource) do
    Ash.data_layer(central_resource).get_many(query, central_resource)
  end

  @spec relationship_query(Ash.record(), association :: Ash.relationship()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  def relationship_query(record, association) do
    Ash.data_layer(Ash.to_resource(record)).relationship_query(record, association)
  end
end
