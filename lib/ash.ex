defmodule Ash do
  alias Ash.Resource.Relationships.{BelongsTo, HasOne, HasMany, ManyToMany}

  @type record :: struct
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type query :: term
  @type resource :: module
  @type error :: struct
  @type side_load_keyword :: Keyword.t()

  def resources() do
    Application.get_env(:ash, :resources) || []
  end

  def relationships(resource) do
    resource.relationships()
  end

  def actions(resource) do
    resource.actions()
  end

  def attributes(resource) do
    resource.attributes()
  end

  def name(resource) do
    resource.name()
  end

  def type(resource) do
    resource.type()
  end

  def data_layer(resource) do
    resource.data_layer()
  end

  def to_resource(%resource{}), do: resource
  def to_resource(resource) when is_atom(resource), do: resource

  ## Datalayer shit TODO move this elsewhere

  @spec get_by_id(resource, id :: any) :: {:ok, record} | {:error, error}
  def get_by_id(resource, id) do
    data_layer = data_layer(resource)

    with {:ok, query} <- data_layer.resource_to_query(resource),
         {:ok, query} <- data_layer.filter(query, :id, id, resource) do
      data_layer.get_one(query, resource)
    end
  end

  @spec resource_to_query(resource) :: query
  def resource_to_query(resource) do
    data_layer(resource).resource_to_query(resource)
  end

  @spec limit(query, limit :: non_neg_integer, resource) :: {:ok, query} | {:error, error}
  def limit(query, limit, resource) do
    data_layer = data_layer(resource)
    data_layer.limit(query, limit, resource)
  end

  @spec offset(query, offset :: non_neg_integer, resource) :: {:ok, query} | {:error, error}
  def offset(query, offset, resource) do
    data_layer = data_layer(resource)
    data_layer.limit(query, offset, resource)
  end

  @spec get_related(record, relationship) :: {:ok, list(record) | record | nil} | {:error, error}
  def get_related(record, %{cardinality: :many} = relationship) do
    case relationship_query(record, relationship) do
      {:ok, query} ->
        get_many(query, to_resource(record))

      {:error, error} ->
        {:error, error}
    end
  end

  def get_related(record, %{cardinality: :one} = relationship) do
    case relationship_query(record, relationship) do
      {:ok, query} ->
        get_one(query, to_resource(record))

      {:error, error} ->
        {:error, error}
    end
  end

  # This is a very hamfisted approach. Later, this will need to be split up
  # into things like "get batch" and whatnot. This will happen when I integrate
  # dataloader
  @spec side_load(list(record), side_load_keyword, resource) ::
          {:ok, list(record)} | {:error, error}
  def side_load(records, side_load_keyword, resource) do
    data_layer(resource).side_load(records, side_load_keyword, resource)
  end

  @spec get_one(query, central_resource :: resource) :: {:ok, record | nil} | {:error, error}
  def get_one(query, central_resource) do
    data_layer(central_resource).get_one(query, central_resource)
  end

  @spec get_many(query, central_resource :: resource) :: {:ok, list(record)} | {:error, error}
  def get_many(query, central_resource) do
    data_layer(central_resource).get_many(query, central_resource)
  end

  @spec relationship_query(record, association :: relationship) ::
          {:ok, list(record)} | {:error, error}
  def relationship_query(record, association) do
    data_layer(record).relationship_query(to_resource(record), association)
  end
end
