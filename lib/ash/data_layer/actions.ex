defmodule Ash.DataLayer.Actions do
  def run_create_action(resource, action, attributes, relationships, params) do
    Ash.Data.create(resource, action, attributes, relationships, params)
  end

  def run_update_action(record, action, attributes, relationships, params) do
    Ash.Data.update(record, action, attributes, relationships, params)
  end

  def run_destroy_action(record, action, params) do
    Ash.Data.delete(record, action, params)
  end

  def run_read_action(resource, _action, params) do
    with {:ok, query} <- Ash.Data.resource_to_query(resource),
         {:ok, filtered_query} <- Ash.Data.filter(resource, query, params),
         {:ok, paginator} <- Ash.DataLayer.Paginator.paginate(resource, filtered_query, params),
         {:ok, found} <- Ash.Data.get_many(paginator.query, resource) do
      {:ok, %{paginator | results: found}}
    end
  end
end
