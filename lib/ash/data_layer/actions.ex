defmodule Ash.DataLayer.Actions do
  def run_get_action(resource, _action, id, _params) do
    Ash.Data.get_by_id(resource, id)
  end

  def run_index_action(resource, _action, params) do
    with {:ok, query} <- Ash.Data.resource_to_query(resource),
         {:ok, paginator} <- Ash.DataLayer.Paginator.paginate(resource, query, params),
         {:ok, found} <- Ash.Data.get_many(paginator.query, resource) do
      {:ok, %{paginator | results: found}}
    end
  end
end
