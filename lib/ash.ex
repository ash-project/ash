defmodule Ash do
  alias Ash.Resource.Relationships.{BelongsTo, HasOne, HasMany, ManyToMany}

  @type record :: struct
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type query :: struct
  @type resource :: module
  @type error :: struct
  @type side_load_keyword :: Keyword.t()

  def resources() do
    Application.get_env(:ash, :resources) || []
  end

  def primary_key(resource) do
    resource.primary_key()
  end

  def relationship(resource, relationship_name) do
    resource.relationship(relationship_name)
  end

  def relationships(resource) do
    resource.relationships()
  end

  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.find(fn action ->
      action.primary? && action.type == type
    end)
  end

  def action(resource, action, type) do
    resource.action(action, type)
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

  def get_authorized(resource, id, params \\ %{}) do
    do_get(resource, id, true, params)
  end

  def get(resource, id, params \\ %{}) do
    do_get(resource, id, false, params)
  end

  defp do_get(resource, id, auth?, params) do
    # TODO: Figure out this interface
    params_with_filter =
      params
      |> Map.put_new(:filter, %{})
      |> Map.update!(:filter, &Map.put(&1, :id, id))

    read =
      if auth? do
        read_authorized(resource, params_with_filter)
      else
        read(resource, params_with_filter)
      end

    case read do
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

  # TODO: params
  def read_authorized(resource, params \\ %{}) do
    do_read(resource, true, params)
  end

  def read(resource, params \\ %{}) do
    do_read(resource, false, params)
  end

  defp do_read(resource, auth?, params) do
    action = Map.get(params, :action) || primary_action(resource, :read)
    Ash.DataLayer.Actions.run_read_action(resource, action, params, auth?)
  end

  # TODO: auth
  def create(resource, attributes, relationships, params \\ %{}) do
    action = Map.get(params, :action) || primary_action(resource, :create)
    Ash.DataLayer.Actions.run_create_action(resource, action, attributes, relationships, params)
  end

  # TODO: auth
  def update(%resource{} = record, attributes, relationships, params \\ %{}) do
    action = Map.get(params, :action) || primary_action(resource, :update)
    Ash.DataLayer.Actions.run_update_action(record, action, attributes, relationships, params)
  end

  # TODO: auth
  def destroy(%resource{} = record, params \\ %{}) do
    action = Map.get(params, :action) || primary_action(resource, :destroy)
    Ash.DataLayer.Actions.run_destroy_action(record, action, params)
  end

  # TODO: Implement a to_resource protocol, like ecto's to query logic
  def to_resource(%resource{}), do: resource
  def to_resource(resource) when is_atom(resource), do: resource

  ## Datalayer shit TODO move this elsewhere
end
