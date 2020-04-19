defmodule Ash.Actions.Read do
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Actions.SideLoad
  require Logger

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
    side_load_filter = Keyword.get(params, :side_load_filter)
    page_params = Keyword.get(params, :page, [])

    filter =
      case filter do
        %Ash.Filter{} -> filter
        filter -> Ash.Filter.parse(resource, filter, api)
      end

    with %Ash.Filter{errors: [], requests: filter_requests} = filter <-
           filter,
         query <- Ash.DataLayer.resource_to_query(resource),
         {:ok, sort} <- Ash.Actions.Sort.process(resource, sort),
         {:ok, sorted_query} <- Ash.DataLayer.sort(query, sort, resource),
         # We parse the query for validation/side_load auth, but don't use it for querying.
         {:ok, _filtered_query} <- Ash.DataLayer.filter(sorted_query, filter, resource),
         {:ok, side_load_requests} <-
           SideLoad.requests(api, resource, side_loads, filter, side_load_filter),
         {:ok, paginator} <-
           Ash.Actions.Paginator.paginate(api, resource, action, sorted_query, page_params),
         %{data: %{data: %{data: data}}, errors: errors} = engine when errors == %{} <-
           do_authorized(
             paginator.query,
             params,
             filter,
             resource,
             api,
             action,
             side_load_requests ++ filter_requests
           ),
         paginator <- %{paginator | results: data} do
      {:ok, SideLoad.attach_side_loads(paginator, engine.data)}
    else
      %{errors: errors} ->
        if (params[:authorization] || [])[:log_final_report?] do
          case errors do
            %{__engine__: errors} ->
              for %Ash.Error.Forbidden{} = forbidden <- List.wrap(errors) do
                Logger.info(Ash.Error.Forbidden.report_text(forbidden))
              end

            _ ->
              :ok
          end
        end

        {:error, errors}

      {:error, error} ->
        if params[:authorization][:log_final_report?] do
          for %Ash.Error.Forbidden{} = forbidden <- List.wrap(error) do
            Logger.info(Ash.Error.Forbidden.report_text(forbidden))
          end
        end

        {:error, error}
    end
  end

  defp do_authorized(query, params, filter, resource, api, action, requests) do
    request =
      Request.new(
        resource: resource,
        rules: action.rules,
        filter: filter,
        action_type: action.type,
        strict_access?: !Ash.Filter.primary_key_filter?(filter),
        data:
          Request.UnresolvedField.data(
            [[:data, :filter]],
            Ash.Filter.optional_paths(filter),
            fn %{data: %{filter: filter}} = data ->
              fetch_filter = Ash.Filter.request_filter_for_fetch(filter, data)

              case Ash.DataLayer.filter(query, fetch_filter, resource) do
                {:ok, final_query} ->
                  Ash.DataLayer.run_query(final_query, resource)

                {:error, error} ->
                  {:error, error}
              end
            end
          ),
        resolve_when_fetch_only?: true,
        path: [:data],
        name: "#{action.type} - `#{action.name}`"
      )

    if params[:authorization] do
      Engine.run(
        [request | requests],
        api,
        user: params[:authorization][:user]
      )
    else
      Engine.run([request | requests], api, fetch_only?: true)
    end
  end
end
