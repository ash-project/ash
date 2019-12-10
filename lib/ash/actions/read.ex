defmodule Ash.Actions.Read do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoader

  def run(api, resource, action, %{authorize?: true} = params) do
    auth_context = %{
      resource: resource,
      action: action,
      params: params
    }

    user = Map.get(params, :user)

    Authorizer.authorize(user, action.authorization_steps, auth_context, fn authorize_data_fun ->
      with {:ok, paginator} <- do_run(resource, action, api, params),
           {:auth, :allow} <-
             {:auth,
              authorize_data_fun.(
                user,
                paginator.results,
                action.authorization_steps,
                auth_context
              )} do
        side_loads = Map.get(params, :side_load, [])
        global_params = Map.take(params, [:authorize?, :user])

        SideLoader.side_load(resource, paginator, side_loads, api, global_params)
      else
        {:error, error} -> {:error, error}
        {:auth, _} -> {:error, :forbidden}
      end
    end)
  end

  def run(api, resource, action, params) do
    case do_run(resource, action, api, params) do
      {:ok, paginator} ->
        side_loads = Map.get(params, :side_load, [])
        global_params = Map.take(params, [:authorize?, :user])

        SideLoader.side_load(resource, paginator, side_loads, api, global_params)

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_run(resource, action, api, params) do
    with query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, Map.get(params, :sort, [])),
         {:ok, filter, authorization} <-
           Ash.Actions.Filter.process(resource, Map.get(params, :filter, %{})),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         {:ok, filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, filtered_query, params),
         {:ok, found} <- Ash.DataLayer.run_query(paginator.query, resource) do
      {:ok, %{paginator | results: found}}
    else
      {:error, error} -> {:error, error}
    end
  end
end
