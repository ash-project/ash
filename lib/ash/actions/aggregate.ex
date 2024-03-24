defmodule Ash.Actions.Aggregate do
  @moduledoc false
  require Ash.Tracer

  def run(api, query, aggregates, opts) do
    query = %{query | api: api}
    {query, opts} = Ash.Actions.Helpers.add_process_context(query.api, query, opts)

    with %{valid?: true} = query <- Ash.Actions.Read.handle_attribute_multitenancy(query) do
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
              action: read_action,
              authorize?: opts[:authorize?]
            }

            Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(query.api), :aggregate],
                                      metadata do
              Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

              with {:ok, query} <- authorize_query(query, opts, agg_authorize?),
                   {:ok, aggregates} <- validate_aggregates(query, aggregates, opts),
                   {:ok, data_layer_query} <-
                     Ash.Query.data_layer_query(%Ash.Query{
                       resource: query.resource,
                       limit: query.limit,
                       offset: query.offset,
                       api: query.api
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
               set_opts(query, [], opts)
             ) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {name, kind, agg_opts}, {:ok, aggregates} ->
        case Ash.Query.Aggregate.new(query.resource, name, kind, set_opts(query, agg_opts, opts)) do
          {:ok, aggregate} ->
            {:cont, {:ok, [aggregate | aggregates]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
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
          Ash.Query.Aggregate.build_query(query, opts)
      end

    Keyword.put(agg_opts, :query, query)
  end
end
