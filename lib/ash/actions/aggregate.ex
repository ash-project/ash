defmodule Ash.Actions.Aggregate do
  @moduledoc false
  require Ash.Tracer

  def run(api, query, aggregates, opts) do
    query = %{query | api: api}
    {query, opts} = Ash.Actions.Helpers.add_process_context(query.api, query, opts)
    action = query.action || Ash.Resource.Info.primary_action!(query.resource, :read)
    opts = Keyword.put_new(opts, :read_action, action.name)

    with {:ok, aggregates} <- validate_aggregates(query, aggregates, opts),
         %{valid?: true} = query <- Ash.Actions.Read.handle_attribute_multitenancy(query) do
      aggregates
      |> Enum.group_by(fn aggregate ->
        agg_authorize? = aggregate.authorize? && opts[:authorize?]

        read_action =
          aggregate.read_action || query.action ||
            Ash.Resource.Info.primary_action!(query.resource, :read).name

        {agg_authorize?, read_action}
      end)
      |> Enum.reduce_while({:ok, %{}}, fn {{agg_authorize?, read_action}, aggregates},
                                          {:ok, acc} ->
        query =
          if query.__validated_for_action__ == read_action do
            query
          else
            Ash.Query.for_read(query, read_action, %{},
              tenant: opts[:tenant],
              actor: opts[:actor],
              authorize?: opts[:authorize?]
            )
          end

        query = %{query | api: api}

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

            with {:ok, query} <- authorize_query(query, opts, agg_authorize?),
                 {:ok, query} <- Ash.Query.data_layer_query(query),
                 {:ok, result} <-
                   Ash.DataLayer.run_aggregate_query(query, aggregates, query.resource) do
              {:cont, {:ok, Map.merge(acc, result)}}
            else
              {:error, error} ->
                {:halt, {:error, error}}
            end
          end
        end
      end)
    end
  end

  defp authorize_query(query, opts, agg_authorize?) do
    if agg_authorize? do
      case query.api.can(query, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             run_queries?: false,
             alter_source?: true
           ) do
        {:ok, true} ->
          {:ok, query}

        {:ok, true, query} ->
          {:ok, query}

        {:ok, false, error} ->
          {:error, error}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, query}
    end
  end

  defp validate_aggregates(query, aggregates, opts) do
    aggregates
    |> Enum.reduce_while({:ok, []}, fn
      %Ash.Query.Aggregate{} = aggregate, {:ok, aggregates} ->
        {:cont, {:ok, [aggregate | aggregates]}}

      {name, kind}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(
               query.resource,
               name,
               kind,
               set_opts([], opts)
             ) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {name, kind, agg_opts}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(query.resource, name, kind, set_opts(agg_opts, opts)) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
  end

  defp set_opts(specified, others) do
    {agg_opts, _} = Ash.Query.Aggregate.split_aggregate_opts(others)
    Keyword.merge(agg_opts, specified)
  end
end
