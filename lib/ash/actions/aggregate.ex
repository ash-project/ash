defmodule Ash.Actions.Aggregate do
  @moduledoc false
  require Ash.Tracer

  def run(api, query, aggregates, opts) do
    query = %{query | api: api}
    {query, opts} = Ash.Actions.Helpers.add_process_context(query.api, query, opts)
    action = query.action || Ash.Resource.Info.primary_action!(query.resource, :read)

    case validate_aggregates(query, aggregates) do
      {:ok, aggregates} ->
        Ash.Tracer.span :action,
                        Ash.Api.Info.span_name(query.api, query.resource, :aggregate),
                        opts[:tracer] do
          metadata = %{
            api: query.api,
            resource: query.resource,
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            aggregates: List.wrap(aggregates),
            actor: opts[:actor],
            tenant: opts[:tenant],
            action: action.name,
            authorize?: opts[:authorize?]
          }

          Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(query.api), :aggregate],
                                    metadata do
            Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)
            query = Map.put(query, :aggregates, Map.new(aggregates, &{&1.name, &1}))

            {aggregate_auth_requests, [], aggregates_in_query} =
              Ash.Query.Aggregate.requests(
                query,
                true,
                opts[:authorize?],
                [],
                []
              )

            query = %{query | aggregates: %{}}
            resource = query.resource

            case aggregate_auth_requests do
              [] ->
                case Ash.Query.data_layer_query(query) do
                  {:ok, query} ->
                    Ash.DataLayer.run_aggregate_query(query, aggregates_in_query, resource)

                  {:error, error} ->
                    {:error, error}
                end

              requests ->
                case Ash.Engine.run(requests,
                       verbose?: opts[:verbose?],
                       actor: opts[:actor],
                       authorize?: opts[:authorize?],
                       api: query.api,
                       resource: query.resource,
                       tracer: opts[:tracer],
                       query: query
                     ) do
                  {:ok, %{data: %{aggregate: %{[] => filter}}}} ->
                    query
                    |> Ash.Query.do_filter(filter)
                    |> Ash.Query.data_layer_query()
                    |> case do
                      {:ok, query} ->
                        Ash.DataLayer.run_aggregate_query(
                          query,
                          aggregates_in_query,
                          resource
                        )

                      {:error, error} ->
                        {:error, error}
                    end

                  {:error, error} ->
                    {:error, Ash.Error.to_ash_error(error)}
                end
            end
          end
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp validate_aggregates(query, aggregates) do
    aggregates
    |> Enum.reduce_while({:ok, []}, fn
      %Ash.Query.Aggregate{} = aggregate, {:ok, aggregates} ->
        {:cont, {:ok, [aggregate | aggregates]}}

      {name, kind}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(query.resource, name, kind) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {name, kind, opts}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(query.resource, name, kind, opts) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
  end
end
