defmodule Ash.Actions.Aggregate do
  @moduledoc false
  require Ash.Tracer

  def run(domain, query, aggregates, opts) do
    query = Ash.Query.new(query)
    query = %{query | domain: domain}
    {query, opts} = Ash.Actions.Helpers.set_context_and_get_opts(query.domain, query, opts)

    aggregates
    |> Enum.group_by(fn
      %Ash.Query.Aggregate{} = aggregate ->
        agg_authorize? = aggregate.authorize? && opts[:authorize?]

        read_action =
          aggregate.read_action || (query.action && query.action.name) ||
            Ash.Resource.Info.primary_action!(query.resource, :read).name

        {agg_authorize?, read_action}

      {_name, _kind} ->
        {!!opts[:authorize?],
         opts[:read_action] || opts[:action] || (query.action && query.action.name) ||
           Ash.Resource.Info.primary_action!(query.resource, :read).name}

      {_name, _kind, agg_opts} ->
        authorize? =
          Keyword.get(agg_opts, :authorize?, true) && opts[:authorize?]

        {authorize?,
         agg_opts[:read_action] || opts[:read_action] || agg_opts[:action] || opts[:action] ||
           (query.action && query.action.name) ||
           Ash.Resource.Info.primary_action!(query.resource, :read).name}
    end)
    |> Enum.reduce_while({:ok, %{}}, fn
      {{agg_authorize?, read_action}, aggregates}, {:ok, acc} ->
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

        query = %{query | domain: domain}

        Ash.Tracer.span :action,
                        fn ->
                          Ash.Domain.Info.span_name(query.domain, query.resource, :aggregate)
                        end,
                        opts[:tracer] do
          metadata = fn ->
            %{
              domain: query.domain,
              resource: query.resource,
              resource_short_name: Ash.Resource.Info.short_name(query.resource),
              aggregates: List.wrap(aggregates),
              actor: opts[:actor],
              tenant: opts[:tenant],
              action: read_action,
              authorize?: opts[:authorize?]
            }
          end

          Ash.Tracer.telemetry_span [
                                      :ash,
                                      Ash.Domain.Info.short_name(query.domain),
                                      :aggregate
                                    ],
                                    metadata do
            Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

            with {:ok, query} <- Ash.Actions.Read.handle_multitenancy(query),
                 {:ok, query} <- authorize_query(query, opts, agg_authorize?),
                 {:ok, aggregates} <- validate_aggregates(query, aggregates, opts),
                 {:ok, data_layer_query} <-
                   Ash.Query.data_layer_query(%Ash.Query{
                     resource: query.resource,
                     limit: query.limit,
                     offset: query.offset,
                     distinct: query.distinct,
                     domain: query.domain,
                     tenant: query.tenant,
                     to_tenant: query.to_tenant,
                     context: query.context
                   }),
                 {:ok, result} <-
                   Ash.DataLayer.run_aggregate_query(
                     data_layer_query,
                     aggregates,
                     query.resource
                   ) do
              {:cont, {:ok, Map.merge(acc, result)}}
            else
              {:error, error} ->
                {:halt, {:error, error}}
            end
          end
        end
    end)
  end

  defp merge_query(left, right) do
    left
    |> Ash.Query.do_filter(right.filter)
    |> Ash.Query.sort(right.sort, prepend?: true)
    |> Ash.Query.distinct_sort(right.distinct_sort, prepend?: true)
    |> Ash.Query.set_tenant(right.tenant)
    |> Ash.Query.set_context(right.context)
  end

  defp authorize_query(query, opts, agg_authorize?) do
    if agg_authorize? do
      case Ash.can(query, opts[:actor],
             return_forbidden_error?: true,
             pre_flight?: false,
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
               Keyword.put(set_opts(query, [], opts), :agg_name, name)
             ) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {name, kind, agg_opts}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(
               query.resource,
               name,
               kind,
               Keyword.put(set_opts(query, agg_opts, opts), :agg_name, name)
             ) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
    |> case do
      {:ok, aggregates} ->
        {:ok,
         Enum.map(aggregates, fn aggregate ->
           Ash.Actions.Read.add_calc_context(
             aggregate,
             opts[:actor],
             opts[:authorize?],
             opts[:tenant],
             opts[:tracer],
             query.domain
           )
         end)}

      other ->
        other
    end
  end

  defp set_opts(query, specified, others) do
    query = Ash.Query.unset(query, [:limit, :offset])
    {agg_opts, _} = Ash.Query.Aggregate.split_aggregate_opts(others)

    agg_opts = Keyword.merge(agg_opts, specified)

    query =
      case agg_opts[:query] do
        %Ash.Query{} = agg_query ->
          merge_query(agg_query, query)

        nil ->
          query

        opts ->
          Ash.Query.Aggregate.build_query(query, nil, opts)
      end

    Keyword.put(agg_opts, :query, query)
  end
end
