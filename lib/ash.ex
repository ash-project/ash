defmodule Ash do
  alias Ash.Resource.Relationships.{BelongsTo, HasOne, HasMany, ManyToMany}

  @type record :: struct
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type query :: struct
  @type resource :: module
  @type error :: struct
  @type side_loads :: Keyword.t()

  def resources() do
    Application.get_env(:ash, :resources) || []
  end

  def primary_key(resource) do
    resource.primary_key()
  end

  def relationship(resource, relationship_name) do
    # TODO: Make this happen at compile time
    Enum.find(resource.relationships(), &(&1.name == relationship_name))
  end

  def relationships(resource) do
    resource.relationships()
  end

  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  def action(resource, name, type) do
    Enum.find(resource.actions(), &(&1.name == name && &1.type == type))
  end

  def actions(resource) do
    resource.actions()
  end

  def attribute(resource, name) do
    Enum.find(resource.attributes, &(&1.name == name))
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

  def get(resource, id, params \\ %{}) do
    # TODO: Figure out this interface
    params_with_filter =
      params
      |> Map.put_new(:filter, %{})
      |> Map.update!(:filter, &Map.put(&1, :id, id))
      |> Map.put(:page, %{limit: 2})

    case read(resource, params_with_filter) do
      {:ok, %{results: [single_result]}} ->
        {:ok, single_result}

      {:ok, %{results: []}} ->
        {:ok, nil}

      {:error, error} ->
        {:error, error}

      {:ok, %{results: results}} when is_list(results) ->
        {:error, :too_many_results}
    end
  end

  def read(resource, params \\ %{}) do
    case Map.get(params, :action) || primary_action(resource, :read) do
      nil ->
        {:error, "no action provided, and no primary action found"}

      action ->
        Ash.DataLayer.Actions.run_read_action(resource, action, params)
    end
  end

  def create(resource, params) do
    case Map.get(params, :action) || primary_action(resource, :create) do
      nil ->
        {:error, "no action provided, and no primary action found"}

      action ->
        Ash.DataLayer.Actions.run_create_action(resource, action, params)
    end
  end

  # # TODO: auth
  # def create(resource, attributes, relationships, params \\ %{}) do
  #   action = Map.get(params, :action) || primary_action(resource, :create)
  #   Ash.DataLayer.Actions.run_create_action(resource, action, attributes, relationships, params)
  # end

  # # TODO: auth
  # def update(%resource{} = record, attributes, relationships, params \\ %{}) do
  #   action = Map.get(params, :action) || primary_action(resource, :update)
  #   Ash.DataLayer.Actions.run_update_action(record, action, attributes, relationships, params)
  # end

  # # TODO: auth
  # def destroy(%resource{} = record, params \\ %{}) do
  #   action = Map.get(params, :action) || primary_action(resource, :destroy)
  #   Ash.DataLayer.Actions.run_destroy_action(record, action, params)
  # end

  # TODO: Implement a to_resource protocol, like ecto's to query logic
  def to_resource(%resource{}), do: resource
  def to_resource(resource) when is_atom(resource), do: resource

  ## Datalayer shit TODO move this elsewhere
end
