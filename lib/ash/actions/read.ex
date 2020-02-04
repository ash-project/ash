defmodule Ash.Actions.Read do
  alias Ash.Engine
  alias Ash.Actions.SideLoad

  def run(api, resource, action, params) do
    transaction_result =
      Ash.DataLayer.transact(resource, fn ->
        do_run(api, resource, action, params)
      end)

    case transaction_result do
      {:ok, value} -> value
      {:error, error} -> {:error, error}
    end
  end

  defp do_run(api, resource, action, params) do
    filter = Keyword.get(params, :filter, [])
    sort = Keyword.get(params, :sort, [])
    side_loads = Keyword.get(params, :side_load, [])
    page_params = Keyword.get(params, :page, [])

    with %Ash.Filter{errors: [], requests: filter_requests} = filter <-
           Ash.Filter.parse(resource, filter, api),
         {:ok, side_load_requests} <- SideLoad.requests(api, resource, side_loads),
         query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, sort),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         # We parse the query for validation, but don't use it here.
         {:ok, _filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, sorted_query, page_params),
         {:ok, %{data: found} = state} <-
           do_authorized(
             paginator.query,
             params,
             filter,
             resource,
             api,
             action,
             side_load_requests ++ filter_requests
           ),
         paginator <- %{paginator | results: found} do
      {:ok, SideLoad.attach_side_loads(paginator, state)}
    else
      %Ash.Filter{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp do_authorized(query, params, filter, resource, api, action, requests) do
    request =
      Ash.Engine.Request.new(
        api: api,
        resource: resource,
        rules: action.rules,
        filter: Ash.Filter.request_filter(filter),
        action_type: action.type,
        optional_state: Ash.Filter.optional_paths(filter),
        fetcher: fn _, state ->
          fetch_filter = Ash.Filter.request_filter_for_fetch(filter, state)

          case Ash.DataLayer.filter(query, fetch_filter, resource) do
            {:ok, final_query} ->
              Ash.DataLayer.run_query(final_query, resource)

            {:error, error} ->
              {:error, error}
          end
        end,
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

      Engine.run(params[:authorization][:user], [request | requests],
        strict_access?: strict_access?,
        log_final_report?: params[:authorization][:log_final_report?] || false
      )
    else
      authorization = params[:authorization] || []

      Engine.run(authorization[:user], [request | requests], fetch_only?: true)
    end
  end
end
