defmodule Ash.Actions.Read do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoad

  def run(api, resource, action, params) do
    filter = Keyword.get(params, :filter, [])
    sort = Keyword.get(params, :sort, [])
    user = Keyword.get(params, :user, [])
    side_loads = Keyword.get(params, :side_load, [])
    page_params = Keyword.get(params, :page, [])

    with %Ash.Filter{errors: []} = filter <-
           Ash.Filter.parse(resource, filter),
         {:ok, side_load_auths} <- SideLoad.process(resource, side_loads, filter),
         {:auth, :authorized} <-
           {:auth, do_authorize(params, action, user, resource, filter, side_load_auths)},
         query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, sort),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         {:ok, filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, filtered_query, page_params),
         {:ok, found} <- Ash.DataLayer.run_query(paginator.query, resource),
         paginator <- %{paginator | results: found} do
      SideLoad.side_load(resource, paginator, side_loads, api, user)
    else
      %Ash.Filter{errors: errors} -> {:error, errors}
      {:auth, :forbidden} -> {:error, :forbidden}
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorize(params, action, user, resource, filter, side_load_auths) do
    if Keyword.get(params, :authorize?, false) do
      auth_request =
        Ash.Authorization.Request.new(
          resource: resource,
          authorization_steps: action.rules,
          filter: filter
        )

      Authorizer.authorize(user, [auth_request | side_load_auths])
    else
      :authorized
    end
  end
end
