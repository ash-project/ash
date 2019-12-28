defmodule Ash.Actions.Read do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoad

  def run(api, resource, action, params) do
    filter = Keyword.get(params, :filter, [])
    sort = Keyword.get(params, :sort, [])
    side_loads = Keyword.get(params, :side_load, [])
    page_params = Keyword.get(params, :page, [])

    # TODO: Going to have to figure out side loads. I don't
    # think that they can actually reasonably share facts :/

    with %Ash.Filter{errors: [], authorizations: filter_auths} = filter <-
           Ash.Filter.parse(resource, filter),
         {:ok, side_load_auths} <- SideLoad.process(resource, side_loads, filter),
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
             action,
             side_load_auths ++ filter_auths
           ),
         paginator <- %{paginator | results: found} do
      # TODO: side loading is a read only, filter based operation, and as such should be covered
      # by the strict checks. Figure out if that is true for sure.
      SideLoad.side_load(resource, paginator, side_loads, api)
    else
      %Ash.Filter{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorized(query, params, filter, resource, action, auths) do
    if params[:authorization] do
      filter_authorization_request =
        Ash.Authorization.Request.new(
          resource: resource,
          authorization_steps: action.authorization_steps,
          filter: filter,
          action_type: action.type,
          fetcher: fn -> Ash.DataLayer.run_query(query, resource) end,
          state_key: :data,
          relationship: [],
          source: "#{action.type} - `#{action.name}`"
        )

      strict_access? =
        case Keyword.fetch(params[:authorization], :strict_access?) do
          {:ok, value} -> value
          :error -> true
        end

      Authorizer.authorize(params[:authorization][:user], [filter_authorization_request | auths],
        strict_access?: strict_access?
      )
    else
      case Ash.DataLayer.run_query(query, resource) do
        {:ok, found} -> {:ok, %{data: found}}
        {:error, error} -> {:error, error}
      end
    end
  end
end
