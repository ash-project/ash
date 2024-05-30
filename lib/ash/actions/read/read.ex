defmodule Ash.Actions.Read do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Error.Invalid.{LimitRequired, PaginationRequired}
  alias Ash.Filter

  require Logger
  require Ash.Flags
  require Ash.Query
  import Ash.Expr
  require Ash.Tracer

  def unpaginated_read(query, action \\ nil, opts \\ []) do
    run(query, action, Keyword.put(opts, :skip_pagination?, true))
  end

  def read_and_return_unpaged(query, action \\ nil, opts \\ []) do
    run(query, action, Keyword.put(opts, :return_unpaged?, true))
  end

  @spec run(Ash.Query.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Page.page() | list(Ash.Resource.record())}
          | {:ok, Ash.Page.page() | list(Ash.Resource.record()), Ash.Query.t()}
          | {:error, term}
  def run(query, action, opts \\ [])

  def run(%{valid?: false, errors: errors}, _action, _opts) do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def run(query, action, opts) do
    query = Ash.Query.new(query)

    domain = query.domain || opts[:domain] || Ash.Resource.Info.domain(query.resource)

    if !domain do
      raise Ash.Error.Framework.AssumptionFailed, message: "got a query without a domain"
    end

    {query, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, query, opts)
    query = Helpers.apply_opts_load(query, opts)

    query = %{query | domain: domain}

    action = get_action(query.resource, action || query.action)

    Ash.Tracer.span :action,
                    Ash.Domain.Info.span_name(query.domain, query.resource, action.name),
                    opts[:tracer] do
      metadata = %{
        domain: query.domain,
        resource: query.resource,
        resource_short_name: Ash.Resource.Info.short_name(query.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.telemetry_span [:ash, Ash.Domain.Info.short_name(query.domain), :read],
                                metadata do
        Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

        run_around_transaction_hooks(query, fn query ->
          case do_run(query, action, opts) do
            {:error, error} ->
              if opts[:tracer] do
                stacktrace =
                  case error do
                    %{stacktrace: %{stacktrace: stacktrace}} ->
                      stacktrace || []

                    _ ->
                      {:current_stacktrace, stacktrace} =
                        Process.info(self(), :current_stacktrace)

                      stacktrace
                  end

                Ash.Tracer.set_handled_error(opts[:tracer], Ash.Error.to_error_class(error),
                  stacktrace: stacktrace
                )
              end

              {:error, error}

            other ->
              other
          end
        end)
      end
    end
  end

  defp run_around_transaction_hooks(%{around_transaction: []} = query, func),
    do: func.(query)

  defp run_around_transaction_hooks(%{around_transaction: [around | rest]} = query, func) do
    query
    |> set_phase(:around_transaction)
    |> around.(fn query ->
      run_around_transaction_hooks(%{query | around_transaction: rest}, func)
    end)
  end

  defp do_run(query, action, opts) do
    {query, opts} =
      if opts[:unsafe_no_authorize?] do
        {Ash.Query.set_context(query, %{private: %{authorize?: false}}),
         Keyword.put(opts, :authorize?, false)}
      else
        {query, opts}
      end

    opts = sanitize_opts(opts, query)
    action = get_action(query.resource, query.action || action)

    query =
      for_read(
        query,
        action,
        actor: opts[:actor],
        authorize?: opts[:authorize?],
        timeout: opts[:timeout],
        tenant: opts[:tenant]
      )

    initial_query = query

    query = add_field_level_auth(query, query.domain, opts)

    query = %{
      query
      | timeout:
          opts[:timeout] || query.timeout || query.action.timeout ||
            Ash.Domain.Info.timeout(query.domain)
    }

    query =
      add_calc_context_to_query(
        query,
        opts[:actor],
        opts[:authorize?],
        query.tenant,
        opts[:tracer],
        query.domain
      )

    relationship? = Map.has_key?(query.context, :accessing_from)

    page_opts =
      if Keyword.has_key?(opts, :page) do
        page_opts(action, opts[:page], relationship?)
      else
        page_opts(action, query.page, relationship?)
      end

    query =
      if page_opts do
        query
        |> Ash.Query.set_context(%{
          initial_limit: query.limit,
          initial_offset: query.offset,
          page_opts:
            unless opts[:inital_data] do
              page_opts
            end,
          initial_query: query,
          query_opts: opts
        })
      else
        query
      end

    opts = Keyword.delete(opts, :page)
    query = Ash.Query.page(query, page_opts)

    query = load_and_select_sort(query)

    pkey = Ash.Resource.Info.primary_key(query.resource)

    missing_pkeys? =
      opts[:initial_data] &&
        Enum.any?(opts[:initial_data], fn record ->
          Enum.any?(Map.take(record, pkey), fn {_, v} -> is_nil(v) end)
        end)

    reuse_values? = Keyword.get(opts, :reuse_values?, false)

    {calculations_in_query, calculations_at_runtime, query} =
      Ash.Actions.Read.Calculations.split_and_load_calculations(
        query.domain,
        query,
        missing_pkeys?,
        Keyword.fetch(opts, :initial_data),
        reuse_values?
      )

    query =
      add_calc_context_to_query(
        query,
        opts[:actor],
        opts[:authorize?],
        query.tenant,
        opts[:tracer],
        query.domain
      )

    query =
      if opts[:initial_data] do
        select = source_fields(query, opts[:lazy?] && opts[:initial_data]) ++ (query.select || [])

        select =
          if reuse_values? do
            remove_already_selected(select, opts[:initial_data])
          else
            select
          end

        query = %{query | select: select}

        if opts[:lazy?] do
          unload_loaded_calculations_and_aggregates(query, opts[:initial_data])
        else
          query
        end
      else
        Ash.Query.ensure_selected(query, source_fields(query))
      end

    {query, stop?} = add_async_limiter(query, calculations_at_runtime, opts)

    try do
      data_result =
        if opts[:initial_data] do
          load(
            opts[:initial_data],
            query,
            calculations_at_runtime,
            calculations_in_query,
            missing_pkeys?,
            opts
          )
        else
          do_read(query, calculations_at_runtime, calculations_in_query, opts)
        end

      data_result =
        case data_result do
          {:ok, result} -> {:ok, result, nil}
          {:ok, result, count} -> {:ok, result, count}
          other -> other
        end

      with {:ok, data, count} <- data_result,
           data = add_tenant(data, query),
           {:ok, data} <-
             load_through_attributes(
               data,
               %{query | calculations: Map.new(calculations_in_query, &{&1.name, &1})},
               query.domain,
               opts[:actor],
               opts[:tracer],
               opts[:authorize?]
             ),
           {:ok, data} <-
             Ash.Actions.Read.Relationships.load(
               data,
               query,
               opts[:lazy?]
             ),
           {:ok, data} <-
             Ash.Actions.Read.Calculations.run(
               data,
               query,
               calculations_at_runtime,
               calculations_in_query
             ),
           {:ok, data} <-
             load_through_attributes(
               data,
               %{
                 query
                 | calculations: Map.new(calculations_at_runtime, &{&1.name, &1}),
                   load_through: Map.delete(query.load_through || %{}, :attribute)
               },
               query.domain,
               opts[:actor],
               opts[:tracer],
               opts[:authorize?],
               false
             ) do
        query =
          query
          # prevent leakage of stale pid as we stop it at the end of reading
          |> clearup_async_limiter()

        data
        |> Helpers.restrict_field_access(query)
        |> add_tenant(query)
        |> attach_fields(opts[:initial_data], initial_query, query, missing_pkeys?)
        |> cleanup_field_auth(query)
        |> add_page(
          query.action,
          count,
          query.sort,
          query,
          opts
        )
        |> add_query(query, opts)
      else
        {:error, %Ash.Query{errors: errors} = query} ->
          {:error, Ash.Error.to_error_class(errors, query: query)}

        {:error, error} ->
          {:error, Ash.Error.to_error_class(error, query: query)}
      end
    after
      if stop? do
        Agent.stop(query.context[:private][:async_limiter])
      end
    end
  end

  defp do_read(%{action: action} = query, calculations_at_runtime, calculations_in_query, opts) do
    maybe_in_transaction(query, opts, fn ->
      with {:ok, %{valid?: true} = query} <- handle_multitenancy(query),
           query <- add_select_if_none_exists(query),
           query <- %{
             query
             | filter:
                 add_calc_context_to_filter(
                   query.filter,
                   opts[:actor],
                   opts[:authorize?],
                   query.tenant,
                   opts[:tracer],
                   query.domain
                 )
           },
           pre_authorization_query <- query,
           {:ok, query} <- authorize_query(query, opts),
           query_before_pagination <- query,
           query <-
             Ash.Actions.Read.Calculations.deselect_known_forbidden_fields(
               query,
               calculations_at_runtime ++ calculations_in_query
             ),
           {:ok, data_layer_calculations} <- hydrate_calculations(query, calculations_in_query),
           {:ok, query} <-
             hydrate_sort(
               query,
               opts[:actor],
               opts[:authorize?],
               query.tenant,
               opts[:tracer],
               query.domain
             ),
           {:ok, relationship_path_filters} <-
             Ash.Filter.relationship_filters(
               query.domain,
               pre_authorization_query,
               opts[:actor],
               query.tenant,
               agg_refs(query, data_layer_calculations ++ [{nil, query.filter}]),
               opts[:authorize?]
             ),
           data_layer_calculations <-
             authorize_calculation_expressions(
               data_layer_calculations,
               query.resource,
               opts[:authorize?],
               relationship_path_filters,
               opts[:actor],
               query.tenant,
               opts[:tracer],
               query.domain
             ),
           query <-
             authorize_loaded_aggregates(
               query,
               relationship_path_filters,
               opts[:actor],
               opts[:authorize?],
               query.tenant,
               opts[:tracer]
             ),
           query <-
             authorize_sorts(
               query,
               relationship_path_filters,
               opts[:actor],
               opts[:authorize?],
               query.tenant,
               opts[:tracer]
             ),
           {:ok, filter} <-
             filter_with_related(
               query,
               opts[:authorize?],
               relationship_path_filters
             ),
           {:ok, filter} <-
             Filter.run_other_data_layer_filters(
               query.domain,
               query.resource,
               filter,
               query.tenant
             ),
           filter <-
             add_calc_context_to_filter(
               filter,
               opts[:actor],
               opts[:authorize?],
               query.tenant,
               opts[:tracer],
               query.domain
             ),
           filter <-
             update_aggregate_filters(
               filter,
               query.resource,
               opts[:authorize?],
               relationship_path_filters,
               opts[:actor],
               query.tenant,
               opts[:tracer],
               query.domain
             ),
           query <- Map.put(query, :filter, filter),
           query <- Ash.Query.unset(query, :calculations),
           query <- add_relationship_count_aggregates(query),
           {%{valid?: true} = query, before_notifications} <- run_before_action(query),
           {:ok, count} <-
             fetch_count(
               query,
               query_before_pagination,
               relationship_path_filters,
               opts
             ),
           {:ok, query} <- paginate(query, action, opts[:skip_pagination?]),
           {:ok, data_layer_query} <-
             Ash.Query.data_layer_query(query, data_layer_calculations: data_layer_calculations),
           {:ok, results} <-
             run_query(
               set_phase(query, :executing),
               data_layer_query,
               %{
                 actor: opts[:actor],
                 tenant: query.tenant,
                 authorize?: opts[:authorize?],
                 domain: query.domain
               },
               !Keyword.has_key?(opts, :initial_data)
             )
             |> Helpers.rollback_if_in_transaction(
               query.resource,
               query
             ),
           :ok <- validate_get(results, query.action, query),
           results <- add_keysets(query, results, query.sort),
           {:ok, results} <- run_authorize_results(query, results),
           {:ok, results, after_notifications} <- run_after_action(query, results),
           {:ok, count} <- maybe_await(count) do
        {:ok, results, count, before_notifications ++ after_notifications}
      else
        {%{valid?: false} = query, before_notifications} ->
          {:error, query, before_notifications}

        {:error, %Ash.Query{} = query} ->
          {:error, query}

        {:ok, %Ash.Query{valid?: false} = query} ->
          {:error, query}

        %Ash.Query{} = query ->
          {:error, query}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp add_relationship_count_aggregates(query) do
    Enum.reduce(query.load, query, fn {relationship_name, related_query}, query ->
      relationship = Ash.Resource.Info.relationship(query.resource, relationship_name)

      related_query =
        case related_query do
          [] -> Ash.Query.new(relationship.destination)
          query -> query
        end

      needs_count? = related_query.page && related_query.page[:count] == true

      if needs_count? do
        related_query =
          Ash.Query.unset(related_query, [
            :sort,
            :distinct,
            :distinct_sort,
            :lock,
            :load,
            :page,
            :aggregates
          ])

        aggregate_name = paginated_relationship_count_aggregate_name(relationship.name)

        query
        |> Ash.Query.aggregate(aggregate_name, :count, relationship.name,
          query: related_query,
          default: 0
        )
      else
        query
      end
    end)
  end

  @doc false
  def paginated_relationship_count_aggregate_name(relationship_name) do
    "__paginated_#{relationship_name}_count__"
  end

  @doc false
  def cleanup_field_auth(records, query, top_level? \\ true)

  def cleanup_field_auth(
        records,
        %{context: %{private: %{loading_relationship?: true}}},
        _top_level?
      ) do
    records
  end

  def cleanup_field_auth(nil, _query, _top_level?), do: nil
  def cleanup_field_auth([], _query, _top_level?), do: []
  def cleanup_field_auth(%Ash.NotLoaded{} = not_loaded, _query, _top_level?), do: not_loaded

  def cleanup_field_auth([%resource{} | _] = records, [], top_level?),
    do: cleanup_field_auth(records, resource |> Ash.Query.new(), top_level?)

  def cleanup_field_auth(%struct{results: results} = page, query, top_level?)
      when struct in [Ash.Page.Keyset, Ash.Page.Offset] do
    %{page | results: cleanup_field_auth(results, query, top_level?)}
  end

  def cleanup_field_auth(records, %{resource: resource} = query, top_level?)
      when is_list(records) do
    records =
      if top_level? do
        records
      else
        query =
          Ash.Query.set_context(query, %{private: %{cleaning_up_field_auth?: true}})

        Helpers.restrict_field_access(records, query)
      end

    records =
      Enum.reduce(query.load, records, fn {name, related_query}, records ->
        Enum.map(records, fn record ->
          case record do
            %Ash.ForbiddenField{} -> record
            %Ash.NotLoaded{} -> record
            nil -> record
            record -> Map.update!(record, name, &cleanup_field_auth(&1, related_query, false))
          end
        end)
      end)

    records =
      Enum.reduce(
        query.load_through[:attribute] || %{},
        records,
        fn {attr_name, further_load}, records ->
          attribute = Ash.Resource.Info.attribute(resource, attr_name)

          Enum.map(records, fn record ->
            case record do
              %Ash.ForbiddenField{} ->
                record

              %Ash.NotLoaded{} ->
                record

              nil ->
                record

              record ->
                Map.update!(record, attribute.name, fn value ->
                  Ash.Type.rewrite(
                    attribute.type,
                    value,
                    [{:cleanup_field_auth, further_load}],
                    attribute.constraints
                  )
                end)
            end
          end)
        end
      )

    Enum.reduce(
      query.load_through[:calculation] || %{},
      records,
      fn {calc_name, further_load}, records ->
        Enum.map(records, fn record ->
          case Map.get(query.calculations, calc_name) do
            %{load: load, type: type, constraints: constraints} when not is_nil(load) ->
              case record do
                %Ash.ForbiddenField{} ->
                  record

                %Ash.NotLoaded{} ->
                  record

                nil ->
                  record

                record ->
                  Map.update!(record, load, fn value ->
                    Ash.Type.rewrite(
                      type,
                      value,
                      [{:cleanup_field_auth, further_load}],
                      constraints
                    )
                  end)
              end

            %{load: nil, name: name, type: type, constraints: constraints} ->
              case record do
                %Ash.ForbiddenField{} ->
                  record

                %Ash.NotLoaded{} ->
                  record

                nil ->
                  record

                record ->
                  Map.update!(record, :calculations, fn calculations ->
                    Map.update!(calculations, name, fn value ->
                      Ash.Type.rewrite(
                        type,
                        value,
                        [{:cleanup_field_auth, further_load}],
                        constraints
                      )
                    end)
                  end)
              end

            nil ->
              record
          end
        end)
      end
    )
  end

  def cleanup_field_auth(record, query, top_level?) do
    record
    |> List.wrap()
    |> cleanup_field_auth(query, top_level?)
    |> Enum.at(0)
  end

  defp agg_refs(query, calculations_in_query) do
    calculations_in_query
    |> Enum.flat_map(fn {_, expr} ->
      Ash.Filter.used_aggregates(expr, :*)
    end)
    |> Enum.concat(Map.values(query.aggregates))
  end

  defp source_fields(query, lazy_for_initial_data \\ nil) do
    query
    |> Ash.Query.accessing([:relationships], false)
    |> then(fn relationships ->
      if lazy_for_initial_data do
        Enum.reject(relationships, fn relationship ->
          Ash.Resource.loaded?(lazy_for_initial_data, relationship, lists: :any)
        end)
      else
        relationships
      end
    end)
    |> Enum.flat_map(fn name ->
      case Ash.Resource.Info.relationship(query.resource, name) do
        %{no_attributes?: true} ->
          []

        %{manual: {module, opts}, source_attribute: source_attribute} ->
          fields =
            module.select(opts)

          if Ash.Resource.Info.attribute(query.resource, source_attribute) do
            [source_attribute | fields]
          else
            fields
          end

        %{source_attribute: source_attribute} ->
          [source_attribute]
      end
    end)
  end

  defp clearup_async_limiter(
         %{context: %{private: %{async_limiter: async_limiter}} = ctx} = query
       )
       when is_pid(async_limiter) do
    Ash.Query.set_context(query, put_in(ctx[:private][:async_limiter], nil))
  end

  defp clearup_async_limiter(query), do: query

  def add_async_limiter(
        %{context: %{private: %{async_limiter: async_limiter}}} = query,
        _context,
        _opts
      )
      when is_pid(async_limiter) do
    {query, false}
  end

  def add_async_limiter(query, calculations_at_runtime, opts) do
    if Enum.count_until(calculations_at_runtime, 2) + Enum.count_until(query.load, 2) >= 2 &&
         !Application.get_env(:ash, :disable_async?) do
      {:ok, limiter} =
        Ash.Actions.Read.AsyncLimiter.start_link(
          opts[:max_concurrency] || System.schedulers_online() * 2
        )

      {Ash.Query.set_context(query, %{private: %{async_limiter: limiter}}), true}
    else
      {query, false}
    end
  end

  defp maybe_in_transaction(query, opts, func) do
    notify? =
      if Process.get(:ash_started_transaction?) do
        false
      else
        Process.put(:ash_started_transaction?, true)
        true
      end

    try do
      cond do
        query.action.transaction? ->
          Ash.DataLayer.transaction(
            [query.resource | query.action.touches_resources],
            func,
            query.timeout,
            %{
              type: :read,
              metadata: %{
                query: query,
                resource: query.resource,
                action: query.action.name
              },
              data_layer_context: query.context[:data_layer]
            }
          )

        query.timeout ->
          {:ok,
           Ash.ProcessHelpers.task_with_timeout(
             func,
             query.resource,
             query.timeout,
             "#{inspect(query.resource)}.#{query.action.name}",
             opts[:tracer]
           )}

        true ->
          {:ok, func.()}
      end
      |> case do
        {:ok, {:ok, result, count, notifications}} ->
          notify_or_store(query, notifications, notify?)

          {:ok, result, count}

        {:ok, {:error, error, notifications}} ->
          notify_or_store(query, notifications, notify?)

          {:error, error}

        {:ok, value} ->
          value

        other ->
          other
      end
    after
      if notify? do
        Process.put(:ash_started_transaction?, false)
      end
    end
  end

  defp notify_or_store(_query, notifications, notify?) do
    if notify? do
      notifications =
        Ash.Notifier.notify(notifications ++ (Process.delete(:ash_notifications) || []))

      Process.put(:ash_notifications, notifications)
    else
      current_notifications = Process.get(:ash_notifications, [])

      Process.put(
        :ash_notifications,
        notifications ++ current_notifications
      )
    end
  end

  defp add_select_if_none_exists(query) do
    if query.select do
      query
    else
      to_select =
        query.resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      Ash.Query.select(query, to_select)
    end
  end

  defp load(
         initial_data,
         query,
         calculations_at_runtime,
         calculations_in_query,
         missing_pkeys?,
         opts
       ) do
    must_be_reselected = List.wrap(query.select) -- Ash.Resource.Info.primary_key(query.resource)

    query =
      Ash.Actions.Read.Calculations.deselect_known_forbidden_fields(
        query,
        calculations_at_runtime ++ calculations_in_query
      )

    if missing_pkeys? ||
         (Enum.empty?(must_be_reselected) && Enum.empty?(query.aggregates) &&
            Enum.empty?(calculations_in_query)) do
      {:ok, initial_data}
    else
      reselect_and_load(
        initial_data,
        query,
        must_be_reselected,
        calculations_in_query,
        opts
      )
    end
  end

  defp reselect_and_load(
         initial_data,
         query,
         must_be_reselected,
         calculations_in_query,
         opts
       ) do
    primary_key = Ash.Resource.Info.primary_key(query.resource)

    filter =
      initial_data
      |> List.wrap()
      |> Enum.map(&Map.take(&1, primary_key))
      |> case do
        [] ->
          false

        [single] ->
          [single]

        multiple ->
          [or: multiple]
      end

    with %{valid?: true} = query <-
           query
           |> Ash.Query.unset([
             :filter,
             :sort,
             :limit,
             :offset,
             :distinct,
             :select,
             :calculations
           ])
           |> Ash.Query.load(calculations_in_query)
           |> Ash.Query.select(must_be_reselected)
           |> Ash.DataLayer.Simple.set_data(initial_data)
           |> Ash.Query.do_filter(filter),
         {:ok, data_layer_calculations} <-
           hydrate_calculations(
             query,
             calculations_in_query
           ),
         {:ok, relationship_path_filters} <-
           Ash.Filter.relationship_filters(
             query.domain,
             %{query | filter: nil},
             opts[:actor],
             query.tenant,
             agg_refs(query, data_layer_calculations),
             opts[:authorize?]
           ),
         data_layer_calculations <-
           authorize_calculation_expressions(
             data_layer_calculations,
             query.resource,
             opts[:authorize?],
             relationship_path_filters,
             opts[:actor],
             query.tenant,
             opts[:tracer],
             query.domain
           ),
         query <-
           authorize_loaded_aggregates(
             query,
             relationship_path_filters,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer]
           ),
         {:ok, data_layer_query} <- Ash.Query.data_layer_query(query),
         {:ok, data_layer_query} <-
           Ash.DataLayer.add_aggregates(
             data_layer_query,
             Map.values(query.aggregates),
             query.resource
           ),
         {:ok, data_layer_query} <-
           Ash.DataLayer.add_calculations(
             data_layer_query,
             data_layer_calculations,
             query.resource
           ),
         {:ok, results} <-
           run_query(
             query,
             data_layer_query,
             %{
               actor: opts[:actor],
               tenant: query.tenant,
               authorize?: false,
               domain: query.domain
             },
             true
           ) do
      results
      |> attach_fields(initial_data, query, query, false, true)
      |> cleanup_field_auth(query)
      |> compute_expression_at_runtime_for_missing_records(query, data_layer_calculations)
      |> case do
        {:ok, result} ->
          {:ok, result, 0}

        {:error, error} ->
          {:error, error}
      end
    end
  end

  defp hydrate_sort(%{sort: empty} = query, _actor, _authorize?, _tenant, _tracer, _domain)
       when empty in [nil, []] do
    {:ok, query}
  end

  defp hydrate_sort(query, actor, authorize?, tenant, tracer, domain) do
    query.sort
    |> List.wrap()
    |> Enum.map(fn {field, direction} ->
      if is_atom(field) do
        case Ash.Resource.Info.field(query.resource, field) do
          %Ash.Resource.Calculation{} = calc -> {calc, direction}
          %Ash.Resource.Aggregate{} = agg -> {agg, direction}
          _field -> {field, direction}
        end
      else
        {field, direction}
      end
    end)
    |> Enum.reduce_while({:ok, []}, fn
      {%Ash.Resource.Calculation{} = resource_calculation, direction}, {:ok, sort} ->
        {module, opts} = resource_calculation.calculation

        case Ash.Query.Calculation.new(
               resource_calculation.name,
               module,
               opts,
               resource_calculation.type,
               resource_calculation.constraints,
               filterable?: resource_calculation.filterable?,
               sortable?: resource_calculation.sortable?,
               sensitive?: resource_calculation.sensitive?,
               load: resource_calculation.load
             ) do
          {:ok, calc} ->
            case hydrate_calculations(query, [calc]) do
              {:ok, [{calc, expression}]} ->
                {:cont,
                 {:ok,
                  [
                    {%{
                       calc
                       | module: Ash.Resource.Calculation.Expression,
                         opts: [expr: expression]
                     }, direction}
                    | sort
                  ]}}

              {:error, error} ->
                {:halt, {:error, error}}
            end

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {%Ash.Query.Calculation{} = calc, direction}, {:ok, sort} ->
        case hydrate_calculations(query, [calc]) do
          {:ok, [{calc, expression}]} ->
            {:cont,
             {:ok,
              [
                {%{
                   calc
                   | module: Ash.Resource.Calculation.Expression,
                     opts: [expr: expression]
                 }, direction}
                | sort
              ]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {%Ash.Resource.Aggregate{} = agg, direction}, {:ok, sort} ->
        case query_aggregate_from_resource_aggregate(query, agg) do
          {:ok, agg} -> {:cont, {:ok, [{agg, direction} | sort]}}
          {:error, error} -> {:halt, {:error, error}}
        end

      {other, direction}, {:ok, sort} ->
        {:cont, {:ok, [{other, direction} | sort]}}
    end)
    |> case do
      {:ok, sort} ->
        sort =
          Enum.map(sort, fn {field, direction} ->
            case field do
              %struct{} = field
              when struct in [
                     Ash.Query.Calculation,
                     Ash.Aggregate.Calculation,
                     Ash.Resource.Calculation,
                     Ash.Resource.Aggregate
                   ] ->
                {add_calc_context(field, actor, authorize?, tenant, tracer, domain), direction}

              field ->
                {field, direction}
            end
          end)

        {:ok, %{query | sort: Enum.reverse(sort)}}

      {:error, error} ->
        {:error, error}
    end
  end

  defp compute_expression_at_runtime_for_missing_records(data, query, data_layer_calculations) do
    if Enum.any?(data, & &1.__metadata__[:private][:missing_from_data_layer]) do
      {require_calculating, rest} =
        data
        |> Stream.with_index()
        |> Stream.map(fn {record, index} ->
          Ash.Resource.put_metadata(record, :private, %{result_index: index})
        end)
        |> Enum.split_with(& &1.__metadata__[:private][:missing_from_data_layer])

      require_calculating
      |> recalculate(query, Enum.map(data_layer_calculations, &elem(&1, 0)))
      |> case do
        {:ok, result} ->
          {:ok,
           result
           |> Enum.concat(rest)
           |> Enum.sort_by(& &1.__metadata__[:private][:result_index])}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, data}
    end
  end

  defp recalculate(require_calculating, query, data_layer_calculations) do
    Ash.Actions.Read.Calculations.run(require_calculating, query, data_layer_calculations, [])
  end

  defp authorize_query(query, opts) do
    if opts[:authorize?] do
      case Ash.can(query, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             pre_flight?: false,
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

  defp unload_loaded_calculations_and_aggregates(query, initial_data) do
    query =
      query
      |> Map.update!(:calculations, fn calculations ->
        keys =
          calculations
          |> Enum.reject(fn {_key, calc} ->
            Ash.Resource.loaded?(initial_data, calc)
          end)
          |> Enum.map(&elem(&1, 0))

        Map.drop(calculations, keys)
      end)
      |> Map.update!(:aggregates, fn aggregates ->
        keys =
          aggregates
          |> Enum.reject(fn {_key, calc} ->
            Ash.Resource.loaded?(initial_data, calc)
          end)
          |> Enum.map(&elem(&1, 0))

        Map.drop(aggregates, keys)
      end)

    query
    |> Map.update!(:load_through, fn load_through ->
      Map.update(load_through, :calculation, %{}, fn calculations ->
        Map.take(calculations, Map.keys(query.calculations))
      end)
    end)
  end

  defp load_through_attributes(
         results,
         query,
         domain,
         actor,
         tracer,
         authorize?,
         attrs? \\ true
       ) do
    query.resource
    |> Ash.Resource.Info.attributes()
    |> Enum.filter(fn %{name: name, type: type, constraints: constraints} ->
      Ash.Type.can_load?(type, constraints) && Ash.Query.selecting?(query, name)
    end)
    |> Enum.map(& &1.name)
    |> Enum.reduce(query.load_through, fn name, load_through ->
      Map.update(load_through, :attribute, %{name => []}, &Map.put_new(&1, name, []))
    end)
    |> Enum.reduce_while({:ok, results}, fn
      {:calculation, load_through}, {:ok, results} ->
        load_through
        |> Map.take(Map.keys(query.calculations))
        |> Enum.reduce_while({:ok, results}, fn {name, load_statement}, {:ok, results} ->
          calculation = Map.get(query.calculations, name)

          values =
            case calculation.load do
              nil ->
                Enum.map(results, &Map.get(&1.calculations, calculation.name))

              load ->
                Enum.map(results, &Map.get(&1, load))
            end

          case calculation.type do
            {:array, _} ->
              Enum.reduce_while(values, {:ok, []}, fn list, {:ok, acc} ->
                case Ash.Type.load(
                       calculation.type,
                       list,
                       load_statement,
                       calculation.constraints,
                       %{
                         domain: domain,
                         actor: actor,
                         tenant: query.tenant,
                         tracer: tracer,
                         authorize?: authorize?
                       }
                     ) do
                  {:ok, new_values} ->
                    {:cont, {:ok, [new_values | acc]}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              end)
              |> case do
                {:ok, list} ->
                  case calculation.load do
                    nil ->
                      {:cont,
                       {:ok,
                        Enum.zip_with(results, Enum.reverse(list), fn record, value ->
                          Map.update!(record, :calculations, fn calculations ->
                            Map.put(calculations, calculation.name, value)
                          end)
                        end)}}

                    load ->
                      {:cont,
                       {:ok,
                        Enum.zip_with(results, Enum.reverse(list), fn record, value ->
                          Map.put(record, load, value)
                        end)}}
                  end

                {:error, error} ->
                  {:halt, {:error, error}}
              end

            _ ->
              case Ash.Type.load(
                     calculation.type,
                     values,
                     load_statement,
                     calculation.constraints,
                     %{
                       domain: domain,
                       actor: actor,
                       tenant: query.tenant,
                       tracer: tracer,
                       authorize?: authorize?
                     }
                   ) do
                {:ok, new_values} ->
                  case calculation.load do
                    nil ->
                      {:cont,
                       {:ok,
                        Enum.zip_with(results, new_values, fn record, value ->
                          Map.update!(record, :calculations, fn calculations ->
                            Map.put(calculations, calculation.name, value)
                          end)
                        end)}}

                    load ->
                      {:cont,
                       {:ok,
                        Enum.zip_with(results, new_values, fn record, value ->
                          Map.put(record, load, value)
                        end)}}
                  end

                {:error, error} ->
                  {:halt, {:error, error}}
              end
          end
        end)
        |> case do
          {:ok, results} ->
            {:cont, {:ok, results}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      {:attribute, load_through}, {:ok, results} when attrs? ->
        load_through
        |> Enum.reduce_while({:ok, results}, fn {name, load_statement}, {:ok, results} ->
          load_statement =
            if is_map(load_statement) and not is_struct(load_statement) do
              Map.to_list(load_statement)
            else
              load_statement
            end

          attribute = Ash.Resource.Info.attribute(query.resource, name)

          values = Enum.map(results, &Map.get(&1, attribute.name))

          case attribute.type do
            {:array, _} ->
              Enum.reduce_while(values, {:ok, []}, fn list, {:ok, acc} ->
                case Ash.Type.load(
                       attribute.type,
                       list,
                       load_statement,
                       attribute.constraints,
                       %{
                         domain: domain,
                         actor: actor,
                         tenant: query.tenant,
                         tracer: tracer,
                         authorize?: authorize?
                       }
                     ) do
                  {:ok, new_values} ->
                    {:cont, {:ok, [new_values | acc]}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              end)
              |> case do
                {:ok, list} ->
                  {:cont,
                   {:ok,
                    Enum.zip_with(results, Enum.reverse(list), fn record, value ->
                      Map.put(record, attribute.name, value)
                    end)}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end

            _ ->
              case Ash.Type.load(
                     attribute.type,
                     values,
                     load_statement,
                     attribute.constraints,
                     %{
                       domain: domain,
                       actor: actor,
                       tenant: query.tenant,
                       tracer: tracer,
                       authorize?: authorize?
                     }
                   ) do
                {:ok, new_values} ->
                  {:cont,
                   {:ok,
                    Enum.zip_with(results, new_values, fn record, value ->
                      Map.put(record, attribute.name, value)
                    end)}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end
          end
        end)
        |> case do
          {:ok, results} ->
            {:cont, {:ok, results}}

          {:error, error} ->
            {:halt, {:error, error}}
        end

      _, {:ok, results} ->
        {:cont, {:ok, results}}
    end)
  end

  @doc false
  def add_calc_context_to_filter(filter, actor, authorize?, tenant, tracer, domain) do
    Ash.Filter.map(filter, fn
      %Ash.Query.Parent{} = parent ->
        %{
          parent
          | expr:
              add_calc_context_to_filter(parent.expr, actor, authorize?, tenant, tracer, domain)
        }

      %Ash.Query.Exists{} = exists ->
        %{
          exists
          | expr:
              add_calc_context_to_filter(exists.expr, actor, authorize?, tenant, tracer, domain)
        }

      %Ash.Query.Ref{attribute: %Ash.Resource.Calculation{}} = ref ->
        raise Ash.Error.Framework.AssumptionFailed,
          message: "unhandled calculation in filter statement #{inspect(ref)}"

      %Ash.Query.Ref{attribute: %Ash.Resource.Aggregate{}} = ref ->
        raise Ash.Error.Framework.AssumptionFailed,
          message: "unhandled calculation in filter statement #{inspect(ref)}"

      %Ash.Query.Ref{
        attribute: %Ash.Query.Calculation{} = calc,
        relationship_path: relationship_path
      } = ref ->
        calc = add_calc_context(calc, actor, authorize?, tenant, tracer, domain)

        if calc.module.has_expression?() do
          expr =
            case calc.module.expression(calc.opts, calc.context) do
              %Ash.Query.Function.Type{} = expr ->
                expr

              expr ->
                {:ok, expr} = Ash.Query.Function.Type.new([expr, calc.type, calc.constraints])
                expr
            end

          {:ok, expr} =
            Ash.Filter.hydrate_refs(
              expr,
              %{
                resource: ref.resource,
                public?: false
              }
            )

          expr =
            Ash.Filter.move_to_relationship_path(expr, relationship_path)

          add_calc_context_to_filter(
            expr,
            actor,
            authorize?,
            tenant,
            tracer,
            domain
          )
        else
          %{ref | attribute: calc}
        end

      %Ash.Query.Ref{attribute: %Ash.Query.Aggregate{} = agg} = ref ->
        %{
          ref
          | attribute: add_calc_context(agg, actor, authorize?, tenant, tracer, domain)
        }

      other ->
        other
    end)
  end

  @doc false
  def handle_multitenancy(query) do
    action_multitenancy = get_action(query.resource, query.action).multitenancy

    case action_multitenancy do
      :enforce ->
        query = handle_attribute_multitenancy(query)

        with :ok <- validate_multitenancy(query) do
          {:ok, query}
        end

      :allow_global ->
        {:ok, handle_attribute_multitenancy(query)}

      :bypass ->
        {:ok, %{query | tenant: nil, to_tenant: nil}}
    end
  end

  defp handle_attribute_multitenancy(query) do
    multitenancy_attribute = Ash.Resource.Info.multitenancy_attribute(query.resource)

    if multitenancy_attribute && query.tenant do
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(query.resource)
      attribute_value = apply(m, f, [query.to_tenant | a])
      Ash.Query.filter(query, ^ref(multitenancy_attribute) == ^attribute_value)
    else
      query
    end
  end

  defp get_action(resource, action) do
    cond do
      action && is_atom(action) ->
        Ash.Resource.Info.action(resource, action)

      action ->
        action

      true ->
        Ash.Resource.Info.primary_action!(resource, :read)
    end
  end

  defp sanitize_opts(opts, query) do
    Keyword.merge(opts, Map.get(query.context, :override_domain_params) || [])
  end

  defp for_read(query, action, opts) do
    if query.__validated_for_action__ == action.name do
      query
    else
      Ash.Query.for_read(query, action.name, %{}, opts)
    end
  end

  defp validate_multitenancy(query) do
    if is_nil(Ash.Resource.Info.multitenancy_strategy(query.resource)) ||
         Ash.Resource.Info.multitenancy_global?(query.resource) || query.tenant do
      :ok
    else
      {:error, Ash.Error.Invalid.TenantRequired.exception(resource: query.resource)}
    end
  end

  defp add_tenant(data, query) do
    if Ash.Resource.Info.multitenancy_strategy(query.resource) do
      Enum.map(data, fn item ->
        %{item | __metadata__: Map.put(item.__metadata__, :tenant, query.tenant)}
      end)
    else
      data
    end
  end

  defp add_query(result, query, opts) do
    if opts[:return_query?] do
      {:ok, result, query}
    else
      {:ok, result}
    end
  end

  @doc false
  def add_page(data, action, count, sort, original_query, opts) do
    cond do
      opts[:skip_pagination?] ->
        data

      action.pagination == false ->
        data

      original_query.page == false ->
        data

      opts[:return_unpaged?] && original_query.page[:limit] ->
        Ash.Page.Unpaged.new(data, opts)

      original_query.page[:limit] ->
        to_page(data, action, count, sort, original_query, opts)

      true ->
        data
    end
  end

  @doc false
  def to_page(data, action, count, sort, original_query, opts) do
    page_opts = original_query.page

    {data, rest} =
      if page_opts[:limit] do
        Enum.split(data, page_opts[:limit])
      else
        {data, []}
      end

    data =
      if page_opts[:before] do
        Enum.reverse(data)
      else
        data
      end

    more? = not Enum.empty?(rest)

    if page_opts[:offset] do
      Ash.Page.Offset.new(data, count, original_query, more?, opts)
    else
      cond do
        action.pagination.keyset? && (page_opts[:after] || page_opts[:before]) ->
          Ash.Page.Keyset.new(data, count, sort, original_query, more?, opts)

        action.pagination.offset? && action.pagination.keyset? ->
          Ash.Page.Offset.new(data, count, original_query, more?, opts)

        action.pagination.offset? ->
          Ash.Page.Offset.new(data, count, original_query, more?, opts)

        true ->
          Ash.Page.Keyset.new(data, count, sort, original_query, more?, opts)
      end
    end
  end

  defp add_keysets(original_query, data, sort) do
    if Enum.any?(
         Ash.Resource.Info.actions(original_query.resource),
         &(&1.type == :read && &1.pagination && &1.pagination.keyset?)
       ) do
      Ash.Page.Keyset.data_with_keyset(data, original_query.resource, sort)
    else
      data
    end
  end

  defp remove_already_selected(fields, %struct{results: results})
       when struct in [Ash.Page.Keyset, Ash.Page.Offset],
       do: remove_already_selected(fields, results)

  defp remove_already_selected(fields, record) when not is_list(record),
    do: remove_already_selected(fields, List.wrap(record))

  defp remove_already_selected(fields, initial_data) do
    Enum.reject(fields, fn field ->
      Enum.any?(initial_data, &Ash.Resource.selected?(&1, field))
    end)
  end

  defp attach_fields(
         data,
         nil,
         _original_query,
         _query,
         _missing_pkeys?
       ) do
    data
  end

  defp attach_fields(
         data_with_selected,
         data,
         original_query,
         query,
         missing_pkeys?,
         no_relationships? \\ false
       ) do
    {aggregates_in_data, aggregates_in_aggregates} =
      original_query.aggregates
      |> Map.values()
      |> Enum.split_with(& &1.load)

    {calculations_in_data, calculations_in_calculations} =
      original_query.calculations
      |> Map.values()
      |> Enum.split_with(& &1.load)

    fields_from_data =
      (original_query.select || []) ++
        Enum.map(aggregates_in_data, & &1.load) ++ Enum.map(calculations_in_data, & &1.load)

    fields_from_data =
      if no_relationships? do
        fields_from_data
      else
        Keyword.keys(original_query.load || []) ++ fields_from_data
      end

    fields_from_aggregates =
      Enum.map(aggregates_in_aggregates, & &1.name)

    fields_from_calculations =
      Enum.map(calculations_in_calculations, & &1.name)

    fields_from_calculations =
      query.calculations
      |> Map.keys()
      |> Enum.filter(&match?({:__ash_fields_are_visible__, _}, &1))
      |> Enum.concat(fields_from_calculations)

    if Enum.empty?(fields_from_calculations) and Enum.empty?(fields_from_aggregates) and
         Enum.empty?(fields_from_data) do
      data
    else
      pkey = Ash.Resource.Info.primary_key(original_query.resource)
      # we have to assume they are all there and in the same order. Not my
      # favorite thing, but no way around it in the short term.
      if Ash.Resource.Info.embedded?(original_query.resource) || Enum.empty?(pkey) ||
           missing_pkeys? do
        Enum.zip_with([data, data_with_selected], fn [record, match] ->
          record
          |> Map.merge(Map.take(match, fields_from_data))
          |> Map.update!(
            :aggregates,
            &Map.merge(
              &1,
              Map.take(match.aggregates, fields_from_aggregates)
            )
          )
          |> Map.update!(
            :calculations,
            &Map.merge(
              &1,
              Map.take(match.calculations, fields_from_calculations)
            )
          )
        end)
      else
        Enum.map(data, fn record ->
          case Enum.find(data_with_selected, fn selected_record ->
                 record.__struct__.primary_key_matches?(record, selected_record)
               end) do
            nil ->
              Ash.Resource.put_metadata(record, :private, %{missing_from_data_layer: true})

            match ->
              record
              |> Map.merge(Map.take(match, fields_from_data))
              |> Map.update!(
                :aggregates,
                &Map.merge(
                  &1,
                  Map.take(match.aggregates, fields_from_aggregates)
                )
              )
              |> Map.update!(
                :calculations,
                &Map.merge(
                  &1,
                  Map.take(match.calculations, fields_from_calculations)
                )
              )
          end
        end)
      end
    end
  end

  defp validate_get([_, _ | _] = results, %{get?: true}, query) do
    {:error,
     Ash.Error.Invalid.MultipleResults.exception(
       count: Enum.count(results),
       at_least?: true,
       query: query
     )}
  end

  defp validate_get(_, _, _), do: :ok

  defp add_calc_context_to_query(query, actor, authorize?, tenant, tracer, domain) do
    %{
      query
      | load:
          Keyword.new(query.load, fn {key, related_query} ->
            case related_query do
              %Ash.Query{} = related_query ->
                {key,
                 add_calc_context_to_query(
                   related_query,
                   actor,
                   authorize?,
                   tenant,
                   tracer,
                   domain
                 )}

              other ->
                load =
                  query.resource
                  |> Ash.Resource.Info.related(key)
                  |> Ash.Query.new(domain: query.domain)
                  |> Ash.Query.load(other)
                  |> add_calc_context_to_query(actor, authorize?, tenant, tracer, domain)

                {key, load}
            end
          end),
        sort:
          Enum.map(query.sort, fn {field, direction} ->
            case field do
              %struct{} = calc
              when struct in [
                     Ash.Query.Calculation,
                     Ash.Aggregate.Calculation,
                     Ash.Resource.Calculation,
                     Ash.Resource.Aggregate
                   ] ->
                {add_calc_context(calc, actor, authorize?, tenant, tracer, domain), direction}

              other ->
                {other, direction}
            end
          end),
        aggregates:
          Map.new(query.aggregates, fn {key, agg} ->
            {key,
             add_calc_context(agg, actor, agg.authorize? && authorize?, tenant, tracer, domain)}
          end),
        calculations:
          Map.new(query.calculations, fn {key, calc} ->
            {key, add_calc_context(calc, actor, authorize?, tenant, tracer, domain)}
          end),
        filter:
          add_calc_context_to_filter(query.filter, actor, authorize?, tenant, tracer, domain)
    }
  end

  @doc false
  def add_calc_context(%Ash.Query.Aggregate{} = agg, actor, authorize?, tenant, tracer, domain) do
    read_action =
      agg.read_action || Ash.Resource.Info.primary_action!(agg.query.resource, :read).name

    query =
      if agg.query.__validated_for_action__ != read_action do
        domain =
          if agg.query.domain do
            agg.query.domain
          else
            Ash.Domain.Info.related_domain(agg.resource, agg.relationship_path, domain)
          end

        agg.query
        |> Ash.Query.set_context(%{private: %{require_actor?: false}})
        |> Ash.Query.for_read(read_action, %{}, domain: domain)
      else
        agg.query
      end

    %{
      agg
      | context:
          Map.merge(
            %{
              actor: actor,
              authorize?: authorize?,
              tenant: tenant,
              tracer: tracer
            },
            agg.context
          ),
        query: add_calc_context_to_query(query, actor, authorize?, tenant, tracer, domain),
        join_filters:
          Map.new(agg.join_filters, fn {key, filter} ->
            {key, add_calc_context_to_filter(filter, actor, authorize?, tenant, tracer, domain)}
          end)
    }
  end

  def add_calc_context(calc, actor, authorize?, tenant, tracer, _domain) do
    %{
      calc
      | context: %{
          calc.context
          | actor: actor,
            authorize?: authorize?,
            tenant: tenant,
            tracer: tracer
        }
    }
  end

  @doc false
  def add_calc_context(calc, context, domain) do
    add_calc_context(
      calc,
      context.actor,
      context.authorize?,
      context.tenant,
      context.tracer,
      domain
    )
  end

  @doc false
  def update_aggregate_filters(
        filter,
        _resource,
        authorize?,
        relationship_path_filters,
        actor,
        tenant,
        tracer,
        domain
      ) do
    if authorize? do
      Filter.update_aggregates(filter, fn aggregate, ref ->
        if aggregate.authorize? do
          authorize_aggregate(
            aggregate,
            relationship_path_filters,
            actor,
            authorize?,
            tenant,
            tracer,
            domain,
            ref.relationship_path
          )
        else
          aggregate
        end
      end)
    else
      filter
    end
  end

  defp add_join_filters(
         current_join_filters,
         aggregate_relationship_path,
         resource,
         path_filters,
         prefix \\ []
       ) do
    aggregate_relationship_path
    |> :lists.droplast()
    |> Ash.Query.Aggregate.subpaths()
    |> Enum.reduce(current_join_filters, fn path, current_join_filters ->
      action =
        resource
        |> Ash.Resource.Info.related(path)
        |> Ash.Resource.Info.primary_action!(:read)
        |> Map.get(:name)

      last_relationship = last_relationship(resource, prefix ++ path)

      case Map.fetch(path_filters, {last_relationship.source, last_relationship.name, action}) do
        {:ok, filter} ->
          Map.update(current_join_filters, path, filter, fn current_filter ->
            Ash.Query.BooleanExpression.new(:and, current_filter, filter)
          end)

        :error ->
          current_join_filters
      end
    end)
  end

  defp maybe_await(%Task{} = task) do
    case Task.await(task) do
      {:__exception__, e, stacktrace} ->
        reraise e, stacktrace

      other ->
        other
    end
  end

  defp maybe_await(other), do: other

  defp fetch_count(
         %{action: action, resource: resource, page: page} = query,
         query_before_pagination,
         relationship_path_filters,
         opts
       ) do
    needs_count? =
      action.pagination && page &&
        (page[:count] == true ||
           (page[:count] != false and action.pagination.countable == :by_default))

    cond do
      Map.has_key?(query.context, :accessing_from) and needs_count? ->
        # Relationship count is fetched by the parent using aggregates, just return nil here
        {:ok, {:ok, nil}}

      needs_count? ->
        with {:ok, filter} <-
               filter_with_related(
                 query,
                 opts[:authorize?],
                 relationship_path_filters
               ),
             query <-
               query
               |> Ash.Query.unset([:sort, :distinct_sort, :lock, :load, :limit, :offset, :page])
               |> Ash.Query.limit(query_before_pagination.limit)
               |> Ash.Query.offset(query_before_pagination.offset)
               |> Map.put(:filter, filter),
             {:ok, data_layer_query} <- Ash.Query.data_layer_query(query) do
          if Ash.DataLayer.in_transaction?(resource) ||
               !Ash.DataLayer.can?(:async_engine, resource) do
            case do_fetch_count(query, data_layer_query) do
              {:ok, count} -> {:ok, {:ok, count}}
              {:error, error} -> {:error, error}
            end
          else
            {:ok,
             Ash.ProcessHelpers.async(
               fn ->
                 do_fetch_count(query, data_layer_query)
               end,
               opts
             )}
          end
        end

      true ->
        {:ok, {:ok, nil}}
    end
  end

  defp run_before_action(query) do
    query =
      query
      |> Ash.Query.put_context(:private, %{in_before_action?: true})
      |> set_phase(:before_action)

    query.before_action
    |> Enum.reduce_while({query, []}, fn before_action, {query, notifications} ->
      case before_action.(query) do
        {%{valid?: false} = query, new_notifications} ->
          {:halt, {query, notifications ++ new_notifications}}

        %{valid?: false} = query ->
          {:halt, {query, notifications}}

        {query, new_notifications} ->
          {:cont, {query, notifications ++ new_notifications}}

        query ->
          {:cont, {query, notifications}}
      end
    end)
    |> then(fn {query, notifications} -> {set_phase(query), notifications} end)
  end

  @doc false
  def run_authorize_results(query, results) do
    query = set_phase(query, :after_action)

    query.authorize_results
    |> Enum.reduce_while({query, {:ok, results}}, fn authorize_results, {query, {:ok, results}} ->
      case authorize_results.(query, results) do
        {:ok, results} ->
          {:cont, {query, {:ok, results}}}

        {:error, error} ->
          {:halt, {query, {:error, error}}}
      end
    end)
    |> elem(1)
  end

  defp run_after_action(query, results) do
    query = set_phase(query, :after_action)

    query.after_action
    |> Enum.reduce_while({query, {:ok, results, []}}, fn after_action,
                                                         {query, {:ok, results, notifications}} ->
      case after_action.(query, results) do
        {:ok, results} ->
          {:cont, {query, {:ok, results, notifications}}}

        {:ok, results, new_notifications} ->
          {:cont, {query, {:ok, results, notifications ++ new_notifications}}}

        {:error, error} ->
          {:halt, {query, {:error, error}}}
      end
    end)
    |> elem(1)
  end

  defp add_field_level_auth(query, domain, opts) do
    if opts[:authorize?] do
      do_add_field_level_auth(query, domain, opts)
    else
      query
    end
  end

  defp do_add_field_level_auth(query, domain, opts) do
    data = %{
      query: query,
      changeset: nil,
      domain: domain,
      resource: query.resource,
      action_input: nil
    }

    query.resource
    |> Ash.Resource.Info.authorizers()
    |> Enum.reduce(query, fn authorizer, query ->
      state =
        Ash.Authorizer.initial_state(
          authorizer,
          opts[:actor],
          query.resource,
          query.action,
          query.domain
        )

      context = Ash.Authorizer.strict_check_context(authorizer, data)

      case Ash.Authorizer.add_calculations(authorizer, query, state, context) do
        {:ok, query, _} ->
          query

        {:error, error} ->
          Ash.Query.add_error(query, error)
      end
    end)
  end

  defp do_fetch_count(query, data_layer_query) do
    with {:ok, %{count: count}} <- run_count_query(query, data_layer_query) do
      {:ok, count}
    end
  end

  defp run_count_query(
         %{
           resource: destination_resource,
           context: %{
             data_layer: %{lateral_join_source: {root_data, path}}
           },
           action: %{
             name: read_action
           }
         } = query,
         data_layer_query
       ) do
    case Ash.Query.Aggregate.new(destination_resource, :count, :count,
           read_action: read_action,
           query: Ash.Query.unset(query, :sort)
         ) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query_with_lateral_join(
          data_layer_query,
          [aggregate],
          root_data,
          destination_resource,
          path
        )

      {:error, error} ->
        {:error, error}
    end
  end

  defp run_count_query(query, data_layer_query) do
    case Ash.Query.Aggregate.new(query.resource, :count, :count) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query(data_layer_query, [aggregate], query.resource)

      {:error, error} ->
        {:error, error}
    end
  end

  def page_opts(action, page_opts, relationship?) do
    cond do
      action.pagination == false ->
        nil

      Keyword.keyword?(page_opts) && !Keyword.has_key?(page_opts, :limit) &&
          action.pagination.default_limit ->
        Keyword.put(page_opts, :limit, action.pagination.default_limit)

      is_nil(page_opts) and action.pagination.required? and not relationship? ->
        if action.pagination.default_limit do
          [limit: action.pagination.default_limit]
        else
          page_opts
        end

      true ->
        page_opts
    end
  end

  @doc false
  def paginate(starting_query, _action, true) do
    {:ok, starting_query}
  end

  def paginate(starting_query, action, _skip?) do
    page_opts = starting_query.page

    cond do
      action.pagination == false && page_opts ->
        {:error, "Pagination is not supported"}

      action.pagination == false ->
        {:ok, starting_query}

      page_opts == false ->
        if action.pagination.required? do
          {:error, PaginationRequired.exception([])}
        else
          {:ok, starting_query}
        end

      page_opts[:limit] ->
        page_opts =
          Keyword.put(
            page_opts || [],
            :limit,
            page_opts[:limit] || action.pagination.default_limit
          )

        case do_paginate(starting_query, action.pagination, page_opts) do
          {:ok, query} ->
            {:ok, query}

          {:error, error} ->
            {:error, error}
        end

      action.pagination.required? ->
        {:error, LimitRequired.exception([])}

      true ->
        {:ok, starting_query}
    end
  end

  defp do_paginate(query, pagination, page_opts) do
    # We want to make 100% sure that there is a stable sort at the end
    # of the sort for pagination
    query =
      if Ash.Actions.Sort.sorting_on_identity?(query) do
        query
      else
        Ash.Query.sort(query, Ash.Resource.Info.primary_key(query.resource))
      end

    paginated =
      cond do
        page_opts[:before] || page_opts[:after] ->
          keyset_pagination(query, pagination, page_opts)

        page_opts[:offset] ->
          limit_offset_pagination(query, pagination, page_opts)

        pagination.offset? && pagination.keyset? ->
          keyset_pagination(query, pagination, page_opts)

        pagination.offset? ->
          limit_offset_pagination(query, pagination, page_opts)

        true ->
          keyset_pagination(query, pagination, page_opts)
      end

    case paginated do
      {:ok, query} ->
        if page_opts[:filter] do
          {:ok, Ash.Query.filter(query, ^page_opts[:filter])}
        else
          {:ok, query}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp keyset_pagination(query, pagination, opts) do
    limited = Ash.Query.limit(query, limit(query, opts[:limit], query.limit, pagination) + 1)

    if opts[:before] || opts[:after] do
      reversed =
        if opts[:before] do
          reversed_sort = Ash.Sort.reverse(limited.sort)
          max_index = Enum.count(reversed_sort) - 1

          inverted_sort_input_indices = Enum.map(query.sort_input_indices, &(max_index - &1))

          limited
          |> Ash.Query.unset(:sort)
          |> Map.put(:sort, reversed_sort)
          |> Map.put(:sort_input_indices, inverted_sort_input_indices)
        else
          limited
        end

      after_or_before =
        if opts[:before] do
          :before
        else
          :after
        end

      case Ash.Page.Keyset.filter(
             query,
             opts[:before] || opts[:after],
             query.sort,
             after_or_before
           ) do
        {:ok, filter} ->
          {:ok, Ash.Query.do_filter(reversed, filter)}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, limited}
    end
  end

  defp load_and_select_sort(query) do
    query.resource
    |> Ash.Resource.Info.actions()
    |> Enum.any?(&match?(%{pagination: %{keyset?: true}}, &1))
    |> if do
      query.sort
      |> Enum.map(&elem(&1, 0))
      |> then(fn load ->
        Ash.Query.load(query, load)
      end)
    else
      query
    end
  end

  defp limit(query, page_size, query_limit, pagination) do
    max_page_size = pagination && pagination.max_page_size

    if query.context[:private][:bypass_max_page_size?] do
      [page_size, query_limit]
      |> Enum.filter(&is_integer/1)
      |> Enum.min(fn -> max_page_size end)
    else
      [page_size, query_limit, max_page_size]
      |> Enum.filter(&is_integer/1)
      |> Enum.min()
    end
  end

  defp limit_offset_pagination(query, pagination, opts) do
    limited = Ash.Query.limit(query, limit(query, opts[:limit], query.limit, pagination) + 1)

    if opts[:offset] do
      {:ok, Ash.Query.offset(limited, opts[:offset])}
    else
      {:ok, limited}
    end
  end

  defp run_query(
         %{context: %{private: %{action_result: result}}} = query,
         _data_layer_query,
         _context,
         load_attributes?
       ) do
    result
    |> Helpers.select(query)
    |> Helpers.load_runtime_types(query, load_attributes?)
    |> case do
      {:ok, result} ->
        Ash.load(result, query, domain: query.domain, reuse_values?: true)

      other ->
        other
    end
  end

  defp run_query(
         %{
           resource: destination_resource,
           context: %{
             data_layer: %{
               lateral_join_source: {root_data, path}
             }
           }
         } = query,
         data_layer_query,
         _context,
         load_attributes?
       ) do
    if query.limit == 0 do
      {:ok, []}
    else
      data_layer_query
      |> Ash.DataLayer.run_query_with_lateral_join(
        root_data,
        destination_resource,
        path
      )
      |> Helpers.select(query)
      |> Helpers.load_runtime_types(query, load_attributes?)
    end
  end

  defp run_query(
         %{action: %{manual: {mod, opts}}} = query,
         data_layer_query,
         context,
         load_attributes?
       ) do
    query
    |> mod.read(data_layer_query, opts, context)
    |> validate_manual_action_return_result!(query.resource, query.action)
    |> Helpers.select(query)
    |> Helpers.load_runtime_types(query, load_attributes?)
  end

  defp run_query(
         %{resource: resource} = query,
         data_layer_query,
         _context,
         load_attributes?
       ) do
    if query.limit == 0 do
      {:ok, []}
    else
      data_layer_query
      |> Ash.DataLayer.run_query(resource)
      |> Helpers.rollback_if_in_transaction(query.resource, query)
      |> Helpers.select(query)
      |> Helpers.load_runtime_types(query, load_attributes?)
    end
  end

  defp validate_manual_action_return_result!({:ok, list} = result, _resource, _)
       when is_list(list) do
    result
  end

  defp validate_manual_action_return_result!({:error, _error} = result, _resource, _) do
    result
  end

  defp validate_manual_action_return_result!(other, resource, action) do
    raise Ash.Error.Framework.InvalidReturnType,
      message: """
      Manual action #{inspect(action.name)} on #{inspect(resource)} returned an invalid result.

      Expected one of the following:

      * {:ok, [list, of, results]}
      * {:error, error}

      Got:

      #{inspect(other)}
      """
  end

  defp hydrate_calculations(
         query,
         calculations_to_add
       ) do
    Enum.reduce_while(calculations_to_add, {:ok, []}, fn calculation, {:ok, calculations} ->
      if Ash.DataLayer.data_layer_can?(query.resource, :expression_calculation) do
        expression = calculation.module.expression(calculation.opts, calculation.context)

        expression =
          Ash.Expr.fill_template(
            expression,
            calculation.context.actor,
            calculation.context.arguments,
            calculation.context.source_context
          )

        case Ash.Filter.hydrate_refs(expression, %{
               resource: query.resource,
               public?: false,
               parent_stack: parent_stack_from_context(query)
             }) do
          {:ok, expression} ->
            {:cont, {:ok, [{calculation, expression} | calculations]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      else
        {:halt,
         {:error,
          "Expression calculations are not supported by #{Ash.Resource.Info.data_layer(query.resource)}"}}
      end
    end)
  end

  defp parent_stack_from_context(%{
         context: %{
           data_layer: %{lateral_join_source: {_, [{%{resource: resource}, _, _, _} | _]}}
         }
       }) do
    [resource]
  end

  defp parent_stack_from_context(_query) do
    []
  end

  defp authorize_calculation_expressions(
         hydrated_calculations,
         resource,
         authorize?,
         relationship_path_filters,
         actor,
         tenant,
         tracer,
         domain
       ) do
    Enum.map(hydrated_calculations, fn {calculation, expression} ->
      {calculation,
       update_aggregate_filters(
         expression,
         resource,
         authorize?,
         relationship_path_filters,
         actor,
         tenant,
         tracer,
         domain
       )}
    end)
  end

  defp authorize_loaded_aggregates(query, path_filters, actor, authorize?, tenant, tracer) do
    Enum.reduce(query.aggregates, query, fn {name, aggregate}, query ->
      aggregate =
        if authorize? && aggregate.authorize? do
          authorize_aggregate(
            aggregate,
            path_filters,
            actor,
            authorize?,
            tenant,
            tracer,
            query.domain
          )
        else
          add_calc_context(aggregate, actor, authorize?, tenant, tracer, query.domain)
        end

      %{query | aggregates: Map.put(query.aggregates, name, aggregate)}
    end)
  end

  defp authorize_sorts(query, path_filters, actor, authorize?, tenant, tracer) do
    Enum.reduce_while(query.sort, {:ok, []}, fn
      {%Ash.Query.Aggregate{} = aggregate, direction}, {:ok, sort} ->
        new_agg =
          if authorize? && aggregate.authorize? do
            authorize_aggregate(
              aggregate,
              path_filters,
              actor,
              authorize?,
              tenant,
              tracer,
              query.domain
            )
          else
            add_calc_context(aggregate, actor, authorize?, tenant, tracer, query.domain)
          end

        {:cont, {:ok, [{new_agg, direction} | sort]}}

      {%Ash.Query.Calculation{
         module: Ash.Resource.Calculation.Expression,
         opts: [expression: expression]
       } = calc, direction},
      {:ok, sort} ->
        new_expr =
          update_aggregate_filters(
            expression,
            query.resource,
            authorize?,
            path_filters,
            actor,
            tenant,
            tracer,
            query.domain
          )

        new_calc = %{calc | opts: [expression: new_expr]}

        {:cont, {:ok, [{new_calc, direction} | sort]}}

      {other, direction}, {:ok, sort} ->
        {:cont, {:ok, [{other, direction} | sort]}}
    end)
    |> case do
      {:ok, reversed_sort} ->
        %{query | sort: Enum.reverse(reversed_sort)}

      {:error, error} ->
        Ash.Query.add_error(query, error)
    end
  end

  defp query_aggregate_from_resource_aggregate(query, resource_aggregate) do
    resource = query.resource
    related_resource = Ash.Resource.Info.related(resource, resource_aggregate.relationship_path)

    read_action =
      resource_aggregate.read_action ||
        Ash.Resource.Info.primary_action!(related_resource, :read).name

    with %{valid?: true} = aggregate_query <-
           Ash.Query.for_read(related_resource, read_action),
         %{valid?: true} = aggregate_query <-
           Ash.Query.Aggregate.build_query(aggregate_query,
             filter: resource_aggregate.filter,
             sort: resource_aggregate.sort
           ),
         {:ok, query_aggregate} <-
           Ash.Query.Aggregate.new(
             resource,
             resource_aggregate.name,
             resource_aggregate.kind,
             path: resource_aggregate.relationship_path,
             query: aggregate_query,
             field: resource_aggregate.field,
             default: resource_aggregate.default,
             filterable?: resource_aggregate.filterable?,
             type: resource_aggregate.type,
             constraints: resource_aggregate.constraints,
             implementation: resource_aggregate.implementation,
             uniq?: resource_aggregate.uniq?,
             read_action: read_action,
             authorize?: resource_aggregate.authorize?,
             join_filters:
               Map.new(resource_aggregate.join_filters, &{&1.relationship_path, &1.filter})
           ) do
      {:ok, Map.put(query_aggregate, :load, resource_aggregate.name)}
    else
      {:error, error} ->
        {:error, error}

      %{errors: errors} ->
        {:error, errors}
    end
  end

  defp authorize_aggregate(
         aggregate,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         ref_path \\ []
       ) do
    aggregate = add_calc_context(aggregate, actor, authorize?, tenant, tracer, domain)
    last_relationship = last_relationship(aggregate.resource, aggregate.relationship_path)

    additional_filter =
      case Map.fetch(
             path_filters,
             {last_relationship.source, last_relationship.name, aggregate.query.action.name}
           ) do
        :error ->
          true

        {:ok, filter} ->
          filter
      end

    with {:ok, filter} <-
           filter_with_related(aggregate.query, authorize?, path_filters),
         filter =
           update_aggregate_filters(
             filter,
             aggregate.query.resource,
             authorize?,
             path_filters,
             actor,
             tenant,
             tracer,
             domain
           ),
         {:ok, field} <-
           aggregate_field_with_related_filters(
             aggregate,
             path_filters,
             actor,
             authorize?,
             tenant,
             tracer,
             domain,
             ref_path
           ) do
      %{
        aggregate
        | query: Ash.Query.filter(%{aggregate.query | filter: filter}, ^additional_filter),
          field: field,
          join_filters:
            add_join_filters(
              aggregate.join_filters,
              aggregate.relationship_path,
              aggregate.resource,
              path_filters
            )
      }
    else
      {:error, error} ->
        raise "Error processing aggregate authorization filter for #{inspect(aggregate)}: #{inspect(error)}"
    end
  end

  defp aggregate_field_with_related_filters(
         %{field: nil},
         _path_filters,
         _actor,
         _authorize?,
         _tenant,
         _tracer,
         _domain,
         _ref_path
       ),
       do: {:ok, nil}

  defp aggregate_field_with_related_filters(
         %{field: %Ash.Query.Calculation{} = field} = agg,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         ref_path
       ) do
    calc = add_calc_context(field, actor, authorize?, tenant, tracer, domain)

    related_resource = Ash.Resource.Info.related(agg.resource, agg.relationship_path)

    if calc.module.has_expression?() do
      expr =
        case calc.module.expression(calc.opts, calc.context) do
          %Ash.Query.Function.Type{} = expr ->
            expr

          expr ->
            {:ok, expr} = Ash.Query.Function.Type.new([expr, calc.type, calc.constraints])
            expr
        end

      {:ok, expr} =
        Ash.Filter.hydrate_refs(
          expr,
          %{
            resource: related_resource,
            public?: false
          }
        )

      expr =
        add_calc_context_to_filter(
          expr,
          actor,
          authorize?,
          tenant,
          tracer,
          domain
        )

      case do_filter_with_related(related_resource, expr, path_filters, ref_path) do
        {:ok, expr} ->
          {:ok, %{field | module: Ash.Resource.Calculation.Expression, opts: [expr: expr]}}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, calc}
    end
  end

  defp aggregate_field_with_related_filters(
         %{field: %Ash.Query.Aggregate{} = field} = agg,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         ref_path
       ) do
    field = add_calc_context(field, actor, authorize?, tenant, tracer, domain)

    if authorize? && field.authorize? do
      {:ok,
       authorize_aggregate(
         field,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         ref_path
       )}
    else
      {:ok, agg}
    end
  end

  defp aggregate_field_with_related_filters(
         aggregate,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         ref_path
       )
       when is_atom(aggregate.field) do
    related_resource = Ash.Resource.Info.related(aggregate.resource, aggregate.relationship_path)

    case Ash.Resource.Info.field(related_resource, aggregate.field) do
      %Ash.Resource.Calculation{} = resource_calculation ->
        {module, opts} = resource_calculation.calculation

        case Ash.Query.Calculation.new(
               resource_calculation.name,
               module,
               opts,
               resource_calculation.type,
               resource_calculation.constraints,
               filterable?: resource_calculation.filterable?,
               sortable?: resource_calculation.sortable?,
               sensitive?: resource_calculation.sensitive?,
               load: resource_calculation.load
             ) do
          {:ok, calculation} ->
            aggregate_field_with_related_filters(
              %{aggregate | field: calculation},
              path_filters,
              actor,
              authorize?,
              tenant,
              tracer,
              domain,
              ref_path
            )

          {:error, error} ->
            {:error, error}
        end

      %Ash.Resource.Aggregate{} = resource_aggregate ->
        agg_related_resource =
          Ash.Resource.Info.related(related_resource, resource_aggregate.relationship_path)

        read_action =
          resource_aggregate.read_action ||
            Ash.Resource.Info.primary_action!(agg_related_resource, :read).name

        with %{valid?: true} = aggregate_query <-
               Ash.Query.for_read(agg_related_resource, read_action),
             %{valid?: true} = aggregate_query <-
               Ash.Query.Aggregate.build_query(aggregate_query,
                 filter: resource_aggregate.filter,
                 sort: resource_aggregate.sort
               ),
             {:ok, query_aggregate} <-
               Ash.Query.Aggregate.new(
                 related_resource,
                 resource_aggregate.name,
                 resource_aggregate.kind,
                 path: resource_aggregate.relationship_path,
                 query: aggregate_query,
                 field: resource_aggregate.field,
                 default: resource_aggregate.default,
                 filterable?: resource_aggregate.filterable?,
                 type: resource_aggregate.type,
                 constraints: resource_aggregate.constraints,
                 implementation: resource_aggregate.implementation,
                 uniq?: resource_aggregate.uniq?,
                 read_action: read_action,
                 authorize?: resource_aggregate.authorize?,
                 join_filters:
                   Map.new(resource_aggregate.join_filters, &{&1.relationship_path, &1.filter})
               ) do
          aggregate_field_with_related_filters(
            %{aggregate | field: query_aggregate},
            path_filters,
            actor,
            authorize?,
            tenant,
            tracer,
            domain,
            ref_path
          )
        else
          {:error, error} ->
            {:error, error}

          %{errors: errors} ->
            {:error, errors}
        end

      _ ->
        {:ok, aggregate.field}
    end
  end

  defp aggregate_field_with_related_filters(
         aggregate,
         _path_filters,
         _actor,
         _authorize?,
         _tenant,
         _tracer,
         _domain,
         _ref_path
       )
       when is_atom(aggregate.field) do
    {:ok, aggregate.field}
  end

  defp filter_with_related(
         query,
         authorize?,
         path_filters
       ) do
    if authorize? do
      do_filter_with_related(query.resource, query.filter, path_filters, [])
    else
      {:ok, query.filter}
    end
  end

  defp do_filter_with_related(
         resource,
         %Ash.Filter{expression: expression} = filter,
         path_filters,
         prefix
       ) do
    case do_filter_with_related(
           resource,
           expression,
           path_filters,
           prefix
         ) do
      {:ok, new_expr} ->
        {:ok, %{filter | expression: new_expr}}

      other ->
        other
    end
  end

  defp do_filter_with_related(
         resource,
         %Ash.Query.BooleanExpression{op: :or, left: left, right: right},
         path_filters,
         prefix
       ) do
    with {:ok, left} <- do_filter_with_related(resource, left, path_filters, prefix),
         {:ok, right} <- do_filter_with_related(resource, right, path_filters, prefix) do
      {:ok, Ash.Query.BooleanExpression.optimized_new(:or, left, right)}
    end
  end

  defp do_filter_with_related(resource, filter_expr, path_filters, prefix) do
    paths_to_global_filter_on =
      filter_expr
      |> Ash.Filter.list_refs()
      |> Enum.filter(&(&1.input? && &1.relationship_path != []))
      |> Enum.map(& &1.relationship_path)
      |> Enum.uniq()

    paths_to_global_filter_on
    |> Enum.reduce_while(
      {:ok, filter_expr},
      fn path, {:ok, filter} ->
        last_relationship =
          Enum.reduce(path, nil, fn
            relationship, nil ->
              Ash.Resource.Info.relationship(resource, relationship)

            relationship, acc ->
              Ash.Resource.Info.relationship(acc.destination, relationship)
          end)

        read_action =
          last_relationship.read_action ||
            Ash.Resource.Info.primary_action!(last_relationship.destination, :read).name

        case Map.get(
               path_filters,
               {last_relationship.source, last_relationship.name, read_action}
             ) do
          nil ->
            {:cont, {:ok, filter}}

          %Ash.Filter{expression: authorization_filter} ->
            {:cont,
             {:ok,
              Ash.Query.BooleanExpression.optimized_new(
                :and,
                filter_expr,
                Ash.Filter.move_to_relationship_path(authorization_filter, path)
              )}}
        end
      end
    )
    |> case do
      {:ok, filter} ->
        {:ok,
         filter
         |> Ash.Filter.map(fn
           %Ash.Query.Exists{at_path: at_path, path: exists_path, expr: exists_expr} = exists ->
             {:ok, new_expr} =
               do_filter_with_related(
                 resource,
                 exists_expr,
                 path_filters,
                 prefix ++ at_path ++ exists_path
               )

             {:halt, %{exists | expr: new_expr}}

           other ->
             other
         end)}

      other ->
        other
    end
  end

  defp last_relationship(resource, list) do
    path = :lists.droplast(list)
    last = List.last(list)

    Ash.Resource.Info.relationship(Ash.Resource.Info.related(resource, path), last)
  end

  defp set_phase(query, phase \\ :preparing)
       when phase in ~w[preparing before_action after_action executing around_transaction]a,
       do: %{query | phase: phase}
end
