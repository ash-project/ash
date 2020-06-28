defmodule Ash.Actions.Read do
  @moduledoc false
  alias Ash.Actions.SideLoad
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Filter
  require Logger

  def run(query, _action, opts \\ []) do
    engine_opts = Keyword.take(opts, [:verbose?, :actor, :authorize?])

    with %{errors: []} <- query,
         {:action, action} when not is_nil(action) <- {:action, action(query, opts)},
         {:ok, requests} <- requests(query, action, opts),
         side_load_requests <- SideLoad.requests(query),
         %{data: %{data: data} = all_data, errors: []} <-
           Engine.run(requests ++ side_load_requests, query.api, engine_opts),
         data_with_side_loads <- SideLoad.attach_side_loads(data, all_data) do
      {:ok, data_with_side_loads}
    else
      {:action, nil} ->
        {:error, "No such action defined, or no default action defined"}

      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp action(query, opts) do
    case opts[:action] do
      %Ash.Resource.Actions.Read{name: name} ->
        Ash.action(query.resource, name, :read)

      nil ->
        Ash.primary_action(query.resource, :read)

      action ->
        Ash.action(query.resource, action, :read)
    end
  end

  defp requests(query, action, opts) do
    filter_requests =
      if Keyword.has_key?(opts, :actor) || opts[:authorize?] do
        Filter.read_requests(query.filter)
      else
        {:ok, []}
      end

    case filter_requests do
      {:ok, filter_requests} ->
        request =
          Request.new(
            resource: query.resource,
            api: query.api,
            query: query,
            action: action,
            data: data_field(opts, filter_requests, query.resource, query.data_layer_query),
            path: [:data],
            name: "#{action.type} - `#{action.name}`"
          )

        {:ok, [request | filter_requests]}

      {:error, error} ->
        {:error, error}
    end
  end

  defp data_field(params, filter_requests, resource, query) do
    if params[:initial_data] do
      List.wrap(params[:initial_data])
    else
      relationship_filter_paths =
        Enum.flat_map(filter_requests, fn request ->
          [request.path ++ [:data], request.path ++ [:authorization_filter]]
        end)

      Request.resolve(
        [[:data, :query] | relationship_filter_paths],
        fn %{data: %{query: ash_query}} = data ->
          with {:ok, filter} <- filter_with_related(relationship_filter_paths, ash_query, data),
               {:ok, filter} <-
                 Filter.run_other_data_layer_filters(
                   ash_query.resource,
                   ash_query.api,
                   filter
                 ),
               {:ok, query} <- Ash.DataLayer.filter(query, filter, resource),
               {:ok, query} <- Ash.DataLayer.limit(query, ash_query.limit, resource),
               {:ok, query} <- Ash.DataLayer.offset(query, ash_query.offset, resource) do
            Ash.DataLayer.run_query(query, resource)
          end
        end
      )
    end
  end

  defp filter_with_related(relationship_filter_paths, ash_query, data) do
    Enum.reduce_while(relationship_filter_paths, {:ok, ash_query.filter}, fn path,
                                                                             {:ok, filter} ->
      case get_in(data, path) do
        nil ->
          {:cont, {:ok, filter}}

        authorization_filter ->
          add_authorization_filter(filter, authorization_filter)
      end
    end)
  end

  defp add_authorization_filter(filter, authorization_filter) do
    case Ash.Filter.add_to_filter(filter, authorization_filter) do
      {:ok, new_filter} ->
        {:cont, {:ok, new_filter}}

      {:error, error} ->
        {:halt, {:error, error}}
    end
  end
end
