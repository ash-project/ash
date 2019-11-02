defmodule Ash.DataLayer.Actions do
  def run_get_action(resource, _action, id, _params) do
    Ash.Data.get_by_id(resource, id)
  end

  def run_create_action(resource, action, attributes, relationships, params) do
    Ash.Data.create(resource, action, attributes, relationships, params)
  end

  def run_update_action(record, action, attributes, relationships, params) do
    Ash.Data.update(record, action, attributes, relationships, params)
  end

  def run_delete_action(record, action, params) do
    Ash.Data.delete(record, action, params)
  end

  def run_index_action(resource, _action, params) do
    with {:ok, query} <- Ash.Data.resource_to_query(resource),
         {:ok, paginator} <- Ash.DataLayer.Paginator.paginate(resource, query, params),
         {:ok, found} <- Ash.Data.get_many(paginator.query, resource) do
      {:ok, %{paginator | results: found}}
    end
  end
end
