defmodule Ash.Actions.Read do
  @moduledoc false
  alias Ash.Actions.SideLoad
  alias Ash.Engine
  alias Ash.Engine.Request
  alias Ash.Filter
  alias Ash.Query.Aggregate
  require Logger

  def run(query, action, opts \\ []) do
    engine_opts = Keyword.take(opts, [:verbose?, :actor, :authorize?])

    with %{errors: []} <- query,
         {:ok, requests} <- requests(query, action, opts),
         side_load_requests <- SideLoad.requests(query),
         %{data: %{data: data} = all_data, errors: []} <-
           Engine.run(requests ++ side_load_requests, query.api, engine_opts),
         data_with_side_loads <- SideLoad.attach_side_loads(data, all_data),
         data_with_aggregates <-
           add_aggregate_values(
             data_with_side_loads,
             query.resource,
             Map.get(all_data, :aggregate_values, %{})
           ) do
      {:ok, data_with_aggregates}
    else
      %{errors: errors} ->
        {:error, Ash.Error.to_ash_error(errors)}

      {:error, error} ->
        {:error, Ash.Error.to_ash_error(error)}
    end
  end

  defp requests(query, action, opts) do
    filter_requests =
      if Keyword.has_key?(opts, :actor) || opts[:authorize?] do
        Filter.read_requests(query.api, query.filter)
      else
        {:ok, []}
      end

    authorizing? = Keyword.has_key?(opts, :actor) || opts[:authorize?]

    {aggregate_auth_requests, aggregate_value_requests, aggregates_in_query} =
      Aggregate.requests(query, authorizing?)

    case filter_requests do
      {:ok, filter_requests} ->
        request =
          Request.new(
            resource: query.resource,
            api: query.api,
            query: query,
            action: action,
            data:
              data_field(
                opts,
                filter_requests,
                aggregate_auth_requests,
                aggregates_in_query,
                query
              ),
            path: [:data],
            name: "#{action.type} - `#{action.name}`"
          )

        {:ok, [request | filter_requests] ++ aggregate_auth_requests ++ aggregate_value_requests}

      {:error, error} ->
        {:error, error}
    end
  end

  defp data_field(
         params,
         filter_requests,
         aggregate_auth_requests,
         aggregates_in_query,
         initial_query
       ) do
    if params[:initial_data] do
      List.wrap(params[:initial_data])
    else
      relationship_filter_paths =
        Enum.map(filter_requests, fn request ->
          request.path ++ [:authorization_filter]
        end)

      aggregate_auth_paths =
        Enum.map(aggregate_auth_requests, fn request ->
          request.path ++ [:authorization_filter]
        end)

      deps = [
        [:data, :query]
        | relationship_filter_paths ++ aggregate_auth_paths
      ]

      Request.resolve(
        deps,
        fn %{data: %{query: ash_query}} = data ->
          query = Ash.Query.unset(initial_query, [:filter, :aggregates, :sort]).data_layer_query

          with {:ok, filter} <- filter_with_related(relationship_filter_paths, ash_query, data),
               {:ok, filter} <-
                 Filter.run_other_data_layer_filters(
                   ash_query.api,
                   ash_query.resource,
                   filter
                 ),
               {:ok, query} <-
                 add_aggregates(
                   query,
                   ash_query,
                   aggregates_in_query,
                   Map.get(data, :aggregate, %{})
                 ),
               {:ok, query} <- Ash.DataLayer.filter(query, filter, ash_query.resource),
               {:ok, query} <- Ash.DataLayer.sort(query, ash_query.sort, ash_query.resource) do
            Ash.DataLayer.run_query(query, ash_query.resource)
          end
        end
      )
    end
  end

  defp add_aggregate_values(results, _resource, aggregate_values) when aggregate_values == %{},
    do: Enum.map(results, &Map.update!(&1, :aggregates, fn agg -> agg || %{} end))

  defp add_aggregate_values(results, resource, aggregate_values) do
    keys_to_aggregates =
      Enum.reduce(aggregate_values, %{}, fn {_name, keys_to_values}, acc ->
        Enum.reduce(keys_to_values, acc, fn {pkey, values}, acc ->
          Map.update(acc, pkey, values, &Map.merge(&1, values))
        end)
      end)

    pkey = Ash.Resource.primary_key(resource)

    Enum.map(results, fn result ->
      aggregate_values = Map.get(keys_to_aggregates, Map.take(result, pkey), %{})
      %{result | aggregates: Map.merge(result.aggregates || %{}, aggregate_values)}
    end)
  end

  defp add_aggregates(data_layer_query, query, aggregates_to_add, aggregate_filters) do
    aggregates_to_add =
      Enum.into(aggregates_to_add, %{}, fn aggregate ->
        {aggregate.name, aggregate}
      end)

    aggregates_to_add
    |> Enum.reduce({aggregates_to_add, aggregate_filters}, fn {name, aggregate},
                                                              {aggregates, aggregate_filters} ->
      case Map.fetch(aggregate_filters, aggregate.relationship_path) do
        {:ok, %{authorization_filter: filter}} ->
          {Map.put(aggregates, name, %{aggregate | authorization_filter: filter}),
           Map.delete(aggregates, aggregate.relationship_path)}

        :error ->
          {aggregates, aggregate_filters}
      end
    end)
    |> elem(0)
    |> Enum.reduce_while({:ok, data_layer_query}, fn {_name, aggregate},
                                                     {:ok, data_layer_query} ->
      case Ash.DataLayer.add_aggregate(data_layer_query, aggregate, query.resource) do
        {:ok, new_query} -> {:cont, {:ok, new_query}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
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
