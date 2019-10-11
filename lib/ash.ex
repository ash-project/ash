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
end
