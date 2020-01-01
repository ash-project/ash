defmodule Ash.Actions.Read do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoad

  def run(api, resource, action, params) do
    filter = Keyword.get(params, :filter, [])
    sort = Keyword.get(params, :sort, [])
    side_loads = Keyword.get(params, :side_load, [])
    page_params = Keyword.get(params, :page, [])

    with %Ash.Filter{errors: [], authorizations: filter_auths} = filter <-
           Ash.Filter.parse(resource, filter, api),
         {:ok, side_load_auths} <- SideLoad.process(api, resource, side_loads, filter),
         query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, sort),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         {:ok, filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, filtered_query, page_params),
         {:ok, %{data: found}} <-
           do_authorized(
             paginator.query,
             params,
             filter,
             resource,
             api,
             action,
             side_load_auths ++ filter_auths
           ),
         paginator <- %{paginator | results: found} do
      SideLoad.side_load(resource, paginator, side_loads, api)
    else
      %Ash.Filter{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorized(query, params, filter, resource, api, action, auths) do
    filter_authorization_request =
      Ash.Authorization.Request.new(
        api: api,
        resource: resource,
        authorization_steps: action.authorization_steps,
        filter: filter,
        action_type: action.type,
        fetcher: fn -> Ash.DataLayer.run_query(query, resource) end,
        must_fetch?: true,
        state_key: :data,
        relationship: [],
        source: "#{action.type} - `#{action.name}`"
      )

    if params[:authorization] do
      strict_access? =
        case Keyword.fetch(params[:authorization], :strict_access?) do
          {:ok, value} -> value
          :error -> true
        end

      Authorizer.authorize(params[:authorization][:user], [filter_authorization_request | auths],
        strict_access?: strict_access?,
        log_final_report?: params[:authorization][:log_final_report?] || false
      )
    else
      authorization = params[:authorization] || []

      Authorizer.authorize(authorization[:user], [filter_authorization_request | auths],
        fetch_only?: true
      )
    end
  end
end
