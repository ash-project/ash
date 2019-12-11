defmodule Ash.Actions.Read do
  alias Ash.Authorization.Authorizer
  alias Ash.Actions.SideLoad

  def run(api, resource, action, params) do
    with {:ok, filter, filter_authorizations} <-
           Ash.Actions.Filter.process(resource, Map.get(params, :filter, [])),
         {:ok, sl_authorizations} <-
           Ash.Actions.SideLoad.process(resource, Map.get(params, :side_load, []), filter) do
      user = Map.get(params, :user)

      precheck_data =
        if Map.get(params, :authorize?, false) do
          authorizations_required =
            sl_authorizations
            |> Enum.into(%{})
            |> Map.put(:filter, filter_authorizations)
            |> Map.put(:request, [{:read, resource, filter}])

          Authorizer.run_precheck(authorizations_required, user)
        else
          :allowed
        end

      side_loads = Map.get(params, :side_load, [])

      with precheck_data when precheck_data != :forbidden <- precheck_data,
           query <- Ash.DataLayer.resource_to_query(resource),
           {:ok, sort} <- Ash.Actions.Sort.process(resource, Map.get(params, :sort, [])),
           {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
           {:ok, filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
           {:ok, paginator} <-
             Ash.Actions.Paginator.paginate(api, resource, action, filtered_query, params),
           {:ok, found} <- Ash.DataLayer.run_query(paginator.query, resource),
           paginator <- %{paginator | results: found} do
        SideLoad.side_load(resource, paginator, side_loads, api, precheck_data)
      else
        :forbidden -> {:error, :forbidden}
        {:error, error} -> {:error, error}
      end
    end
  end
end
