# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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
        action =
          read_action ||
            Ash.Resource.Info.primary_action!(query.resource, :read).name

        query =
          if query.__validated_for_action__ == action do
            query
          else
            Ash.Query.for_read(query, action, %{},
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
                 {:ok, %{valid?: true} = query} <-
                   authorize_query(query, opts, agg_authorize?),
                 {:ok, aggregates} <- validate_aggregates(query, aggregates, opts),
                 {:ok, aggregates} <-
                   authorize_aggregate_fields(
                     query,
                     aggregates,
                     opts,
                     agg_authorize? && Keyword.get(opts, :authorize_fields?, false)
                   ) do
              # Group aggregates by bypass vs tenant-specific
              {bypass_aggs, tenant_aggs} =
                Enum.split_with(aggregates, &(&1.multitenancy == :bypass))

              # Run both groups and merge results
              results =
                Enum.reduce_while(
                  [
                    {bypass_aggs,
                     Map.merge(query.context || %{}, %{
                       shared: %{private: %{multitenancy: :bypass_all}}
                     })},
                    {tenant_aggs, query.context}
                  ],
                  {:ok, %{}},
                  fn
                    {[], _context}, acc ->
                      {:cont, acc}

                    {aggs, context}, {:ok, results_acc} ->
                      with {:ok, data_layer_query} <-
                             Ash.Query.data_layer_query(%Ash.Query{
                               action: Ash.Resource.Info.action(query.resource, read_action),
                               resource: query.resource,
                               limit: query.limit,
                               offset: query.offset,
                               distinct: query.distinct,
                               distinct_sort: query.distinct_sort,
                               sort: query.sort,
                               domain: query.domain,
                               tenant: query.tenant,
                               filter: query.filter,
                               to_tenant: query.to_tenant,
                               context: context
                             }),
                           {:ok, group_results} <-
                             Ash.DataLayer.run_aggregate_query(
                               data_layer_query,
                               aggs,
                               query.resource
                             ) do
                        {:cont, {:ok, Map.merge(results_acc, group_results)}}
                      else
                        {:error, error} -> {:halt, {:error, error}}
                      end
                  end
                )

              case results do
                {:ok, merged} -> {:cont, {:ok, Map.merge(acc, merged)}}
                {:error, error} -> {:halt, {:error, error}}
              end
            else
              {:ok, %Ash.Query{} = query} ->
                {:halt, {:error, Ash.Error.to_error_class(query)}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
          end
        end
    end)
  end

  defp authorize_aggregate_fields(_query, aggregates, _opts, false), do: {:ok, aggregates}

  defp authorize_aggregate_fields(query, aggregates, opts, true) do
    aggregates
    |> Enum.reduce_while({:ok, []}, fn aggregate, {:ok, acc} ->
      case authorize_aggregate_field(query, aggregate, opts) do
        {:ok, aggregate} -> {:cont, {:ok, [aggregate | acc]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, aggregates} -> {:ok, Enum.reverse(aggregates)}
      other -> other
    end
  end

  defp authorize_aggregate_field(_query, %{field: nil} = aggregate, _opts), do: {:ok, aggregate}

  defp authorize_aggregate_field(query, aggregate, opts) do
    target_resource = Ash.Resource.Info.related(query.resource, aggregate.relationship_path)
    field_name = referenced_field_name(aggregate.field)

    cond do
      Ash.Policy.Info.field_policies(target_resource) == [] ->
        {:ok, aggregate}

      is_nil(field_name) || is_nil(Ash.Resource.Info.field(target_resource, field_name)) ->
        {:ok, aggregate}

      true ->
        read_action =
          aggregate.read_action || Ash.Resource.Info.primary_action!(target_resource, :read).name

        subject =
          Ash.Query.for_read(target_resource, read_action, %{},
            actor: opts[:actor],
            tenant: opts[:tenant],
            authorize?: true
          )

        case Ash.Can.evaluate_field_policies(subject, query.domain, opts[:actor], [field_name],
               tenant: opts[:tenant],
               domain: query.domain,
               run_queries?: false
             ) do
          {:ok, results} ->
            case Map.get(results, field_name) do
              result when result in [true, nil] ->
                {:ok, aggregate}

              false ->
                {:error, Ash.Error.Forbidden.exception([])}

              {:filter, expr} ->
                base = aggregate.query || Ash.Query.new(target_resource)
                {:ok, %{aggregate | query: Ash.Query.do_filter(base, expr)}}
            end

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp referenced_field_name(field) when is_atom(field), do: field
  defp referenced_field_name(%Ash.Query.Aggregate{name: name}) when is_atom(name), do: name
  defp referenced_field_name(%Ash.Query.Calculation{name: name}) when is_atom(name), do: name

  defp referenced_field_name(%Ash.Query.Calculation{calc_name: name}) when is_atom(name),
    do: name

  defp referenced_field_name(_), do: nil

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
        Enum.reduce_while(aggregates, {:ok, []}, fn aggregate, {:ok, aggregates} ->
          if Ash.DataLayer.data_layer_can?(aggregate.resource, {:query_aggregate, aggregate.kind}) do
            aggregate =
              Ash.Actions.Read.add_calc_context(
                aggregate,
                opts[:actor],
                opts[:authorize?],
                opts[:tenant],
                opts[:tracer],
                query.domain,
                query.resource,
                source_context: query.context
              )

            {:cont, {:ok, [aggregate | aggregates]}}
          else
            {:halt,
             {:error,
              Ash.Error.Query.AggregatesNotSupported.exception(
                resource: aggregate.resource,
                feature: "using",
                type: :query_aggregate
              )}}
          end
        end)

      other ->
        other
    end
  end

  defp set_opts(query, specified, others) do
    {agg_opts, _} = Ash.Query.Aggregate.split_aggregate_opts(others)

    agg_opts = Keyword.merge(agg_opts, specified)

    query =
      case agg_opts[:query] do
        %Ash.Query{} = agg_query ->
          agg_query

        nil ->
          Ash.Query.new(query.resource)

        opts ->
          Ash.Query.Aggregate.build_query(Ash.Query.new(query.resource), nil, opts)
      end

    Keyword.put(agg_opts, :query, query)
  end
end
