defmodule Ash.Actions.Read do
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Actions.SideLoad
  require Logger

  def run(api, resource, action, params) do
    filter = Keyword.get(params, :filter, [])
    sort = Keyword.get(params, :sort, [])
    side_loads = Keyword.get(params, :side_load, [])
    side_load_filter = Keyword.get(params, :side_load_filter)
    page_params = Keyword.get(params, :page, [])

    action =
      if is_atom(action) and not is_nil(action) do
        Ash.action(resource, action, :read)
      else
        action
      end

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
         %{data: %{data: %{data: data}}, errors: errors, authorized?: true} = engine
         when errors == %{} <-
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
      %Ash.Filter{errors: errors} ->
        {:error, Ash.to_ash_error(errors)}

      %Ash.Engine{errors: errors} ->
        errors =
          Enum.flat_map(errors, fn {path, errors} ->
            Enum.map(errors, &Map.put(&1, :path, path))
          end)

        {:error, Ash.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.to_ash_error(error)}
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
          Request.resolve(
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
        user: params[:authorization][:user],
        bypass_strict_access?: params[:bypass_strict_access?],
        verbose?: params[:verbose?]
      )
    else
      Engine.run([request | requests], api, fetch_only?: true, verbose?: params[:verbose?])
    end
  end
end
