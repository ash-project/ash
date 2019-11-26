defmodule Ash.Data do
  # @spec create(Ash.resource(), Ash.action(), Ash.attributes(), Ash.relationships(), Ash.params()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def create(resource, action, attributes, relationships, params) do
  #   Ash.data_layer(resource).create(resource, action, attributes, relationships, params)
  # end

  # @spec update(Ash.record(), Ash.action(), Ash.attributes(), Ash.relationships(), Ash.params()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def update(%resource{} = record, action, attributes, relationships, params) do
  #   Ash.data_layer(resource).update(record, action, attributes, relationships, params)
  # end

  # @spec delete(Ash.record(), Ash.action(), Ash.params()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def delete(%resource{} = record, action, params) do
  #   Ash.data_layer(resource).delete(record, action, params)
  # end

  # @spec append_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def append_related(%resource{} = record, relationship, resource_identifiers) do
  #   Ash.data_layer(resource).append_related(record, relationship, resource_identifiers)
  # end

  # @spec delete_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def delete_related(%resource{} = record, relationship, resource_identifiers) do
  #   Ash.data_layer(resource).delete_related(record, relationship, resource_identifiers)
  # end

  # @spec replace_related(Ash.record(), Ash.relationship(), Ash.resource_identifiers()) ::
  #         {:ok, Ash.record()} | {:error, Ash.error()}
  # def replace_related(%resource{} = record, relationship, resource_identifiers) do
  #   Ash.data_layer(resource).replace_related(record, relationship, resource_identifiers)
  # end

  @spec resource_to_query(Ash.resource()) :: Ash.query()
  def resource_to_query(resource) do
    Ash.data_layer(resource).resource_to_query(resource)
  end

  @spec filter(Ash.resource(), Ash.query(), Ash.params()) ::
          {:ok, Ash.query()} | {:error, Ash.error()}
  # TODO This is a really dumb implementation of this.
  def filter(resource, query, params) do
    data_layer = Ash.data_layer(resource)

    filtered_query =
      params
      |> Map.get(:filter, %{})
      |> Enum.reduce(query, fn {key, value}, query ->
        case query do
          {:error, error} ->
            {:error, error}

          query ->
            case data_layer.filter(query, key, value, resource) do
              {:ok, query} -> query
              {:error, query} -> {:error, query}
            end
        end
      end)

    {:ok, filtered_query}
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

  # @spec get_related(Ash.record(), Ash.relationship()) ::
  #         {:ok, list(Ash.record()) | Ash.record() | nil} | {:error, Ash.error()}
  # def get_related(record, %{cardinality: :many} = relationship) do
  #   case relationship_query(record, relationship) do
  #     {:ok, query} ->
  #       get_many(query, Ash.to_resource(record))

  #     {:error, error} ->
  #       {:error, error}
  #   end
  # end

  # def get_related(record, %{cardinality: :one} = relationship) do
  #   case relationship_query(record, relationship) do
  #     {:ok, query} ->
  #       get_one(query, Ash.to_resource(record))

  #     {:error, error} ->
  #       {:error, error}
  #   end
  # end

  @spec run_query(Ash.query(), central_resource :: Ash.resource()) ::
          {:ok, list(Ash.record())} | {:error, Ash.error()}
  def run_query(query, central_resource) do
    Ash.data_layer(central_resource).run_query(query, central_resource)
  end
end
