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
         {:ok, auth_callback} <- do_authorize(params, side_load_auths ++ filter_auths),
         query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, sort),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         {:ok, filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, filtered_query, page_params),
         {:ok, found} <- Ash.DataLayer.run_query(paginator.query, resource),
         :ok <- auth_callback.(found),
         paginator <- %{paginator | results: found} do
      # TODO: side loading is a read only, filter based operation, and as such should be covered
      # by the strict checks. Figure out if that is true for sure.
      SideLoad.side_load(resource, paginator, side_loads, api)
    else
      %Ash.Filter{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorize(params, auths) do
    if params[:authorization] do
      strict_access =
        case Keyword.fetch(params[:authorization], :strict_access?) do
          {:ok, value} -> value
          :error -> true
        end

      auths = Enum.map(auths, fn auth -> %{auth | strict_access?: strict_access} end)

      Authorizer.authorize(params[:authorization][:user], %{}, auths)
    else
      {:ok, fn _ -> :ok end}
    end
  end
end
