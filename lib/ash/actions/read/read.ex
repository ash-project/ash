# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Read do
  @moduledoc false

  alias Ash.Actions.Helpers
  alias Ash.Error.Invalid.{LimitRequired, NonCountableAction, PaginationRequired}
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

    query =
      if opts[:initial_data] do
        Ash.Query.set_context(query, %{query_for: :load})
      else
        query
      end

    {query, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, query, opts)
    query = Helpers.apply_opts_load(query, opts)

    query = %{query | domain: domain}

    action = get_action(query.resource, action || query.action)

    try do
      tracer =
        if opts[:initial_data] do
          nil
        else
          opts[:tracer]
        end

      Ash.Tracer.span :action,
                      fn ->
                        Ash.Domain.Info.span_name(query.domain, query.resource, action.name)
                      end,
                      tracer do
        metadata = fn ->
          %{
            domain: query.domain,
            resource: query.resource,
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            actor: opts[:actor],
            tenant: opts[:tenant],
            action: action.name,
            authorize?: opts[:authorize?]
          }
        end

        Ash.Tracer.telemetry_span [:ash, Ash.Domain.Info.short_name(query.domain), :read],
                                  metadata,
                                  skip?: !!opts[:initial_data] do
          Ash.Tracer.set_metadata(tracer, :action, metadata)

          run_around_transaction_hooks(query, fn query ->
            case do_run(query, action, opts) do
              {:error, error} ->
                error =
                  Ash.Error.to_error_class(
                    error,
                    bread_crumbs: "Error returned from: #{inspect(query.resource)}.#{action.name}"
                  )

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
    rescue
      e ->
        reraise Ash.Error.to_error_class(e,
                  query: query,
                  stacktrace: __STACKTRACE__,
                  bread_crumbs: [
                    "Exception raised in: #{inspect(query.resource)}.#{action.name}"
                  ]
                ),
                __STACKTRACE__
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

  defp run_before_transaction_hooks(%{before_transaction: []} = query) do
    {:ok, query}
  end

  defp run_before_transaction_hooks(query) do
    Enum.reduce_while(
      query.before_transaction,
      {:ok, query},
      fn before_transaction, {:ok, query} ->
        tracer = query.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: query.domain,
            resource: query.resource,
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            actor: query.context[:private][:actor],
            tenant: query.tenant,
            action: query.action && query.action.name,
            authorize?: query.context[:private][:authorize?]
          }
        end

        result =
          Ash.Tracer.span :before_transaction,
                          "before_transaction",
                          tracer do
            Ash.Tracer.set_metadata(tracer, :before_transaction, metadata)

            Ash.Tracer.telemetry_span [:ash, :before_transaction], metadata do
              before_transaction.(query)
            end
          end

        case result do
          %Ash.Query{} = query ->
            {:cont, {:ok, query}}

          {:error, error} ->
            {:halt, {:error, error}}

          other ->
            raise """
            Invalid return value from before_transaction hook. Expected one of:

            * %Ash.Query{}
            * {:error, error}

            Got:

            #{inspect(other)}
            """
        end
      end
    )
  end

  defp run_after_transaction_hooks(result, %{after_transaction: []} = _query) do
    result
  end

  defp run_after_transaction_hooks(result, query) do
    query.after_transaction
    |> Enum.reduce(
      result,
      fn after_transaction, result ->
        tracer = query.context[:private][:tracer]

        metadata = fn ->
          %{
            domain: query.domain,
            resource: query.resource,
            resource_short_name: Ash.Resource.Info.short_name(query.resource),
            actor: query.context[:private][:actor],
            tenant: query.tenant,
            action: query.action && query.action.name,
            authorize?: query.context[:private][:authorize?]
          }
        end

        Ash.Tracer.span :after_transaction,
                        "after_transaction",
                        tracer do
          Ash.Tracer.set_metadata(tracer, :after_transaction, metadata)

          Ash.Tracer.telemetry_span [:ash, :after_transaction], metadata do
            after_transaction.(query, result)
          end
        end
      end
    )
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
      if action.type == :read do
        for_read(
          query,
          action,
          opts[:initial_data],
          actor: opts[:actor],
          authorize?: opts[:authorize?],
          timeout: opts[:timeout],
          tenant: opts[:tenant]
        )
      else
        query
      end

    initial_query = query
    query = add_field_level_auth(query, query.domain, opts)

    query = %{
      query
      | timeout:
          opts[:timeout] || query.timeout || (query.action && query.action.timeout) ||
            Ash.Domain.Info.timeout(query.domain)
    }

    query =
      add_calc_context_to_query(
        query,
        opts[:actor],
        opts[:authorize?],
        query.tenant,
        opts[:tracer],
        query.domain,
        expand?: false,
        parent_stack: parent_stack_from_context(query.context),
        source_context: query.context
      )

    relationship? = Map.has_key?(query.context, :accessing_from)

    page_opts =
      if Keyword.has_key?(opts, :page) do
        page_opts(action, opts[:page], relationship?)
      else
        page_opts(action, query.page, relationship?)
      end

    opts = Keyword.delete(opts, :page)

    query = Ash.Query.page(query, page_opts)

    query =
      if opts[:initial_data] do
        query
      else
        load_and_select_sort(query, page_opts)
      end

    query = add_relationship_count_aggregates(query)

    pkey = Ash.Resource.Info.primary_key(query.resource)

    # we should probably lazily check this
    missing_pkeys? =
      Enum.empty?(pkey) ||
        (opts[:initial_data] &&
           Enum.any?(opts[:initial_data], fn record ->
             Enum.any?(Map.take(record, pkey), fn {_, v} -> is_nil(v) end)
           end))

    reuse_values? = Keyword.get(opts, :reuse_values?, false)

    case Ash.Actions.Read.Calculations.split_and_load_calculations(
           query.domain,
           query,
           missing_pkeys?,
           Keyword.fetch(opts, :initial_data),
           reuse_values?,
           opts[:authorize?]
         ) do
      {:error, %Ash.Query{errors: errors} = query} ->
        {:error, Ash.Error.to_error_class(errors, query: query)}

      {:error, error} ->
        {:error, Ash.Error.to_error_class(error)}

      {:ok, calculations_in_query, calculations_at_runtime, query} ->
        query =
          add_calc_context_to_query(
            query,
            opts[:actor],
            opts[:authorize?],
            query.tenant,
            opts[:tracer],
            query.domain,
            expand?: false,
            parent_stack: parent_stack_from_context(query.context),
            source_context: query.context
          )

        calculations_at_runtime =
          Enum.map(
            calculations_at_runtime,
            &add_calc_context(
              &1,
              opts[:actor],
              opts[:authorize?],
              query.tenant,
              opts[:tracer],
              query.domain,
              query.resource,
              parent_stack: parent_stack_from_context(query.context),
              source_context: query.context
            )
          )

        calculations_in_query =
          Enum.map(
            calculations_in_query,
            &add_calc_context(
              &1,
              opts[:actor],
              opts[:authorize?],
              query.tenant,
              opts[:tracer],
              query.domain,
              query.resource,
              parent_stack: parent_stack_from_context(query.context),
              source_context: query.context
            )
          )

        source_fields =
          if !opts[:initial_data] do
            source_fields(query)
          end

        query =
          if opts[:initial_data] do
            select =
              source_fields(query, opts[:lazy?] && opts[:initial_data]) ++ (query.select || [])

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
            Ash.Query.ensure_selected(query, source_fields)
          end

        {query, stop?} = add_async_limiter(query, calculations_at_runtime, opts)

        if opts[:data_layer_query?] do
          data_layer_query(
            query,
            calculations_at_runtime,
            calculations_in_query,
            source_fields,
            opts
          )
        else
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
                do_read(
                  query,
                  calculations_at_runtime,
                  calculations_in_query,
                  source_fields,
                  opts
                )
              end

            {data_result, query_ran} =
              case data_result do
                {:ok, _result, _count, _calculations_at_runtime, _calculations_in_query, query} =
                    data_result ->
                  {data_result, query}

                {{:error, _} = data_result, query} ->
                  {data_result, query}

                data_result ->
                  {data_result, query}
              end

            query = Ash.Query.set_context(query, %{shared: query_ran.context[:shared]})

            with {:ok, data, count, calculations_at_runtime, calculations_in_query, new_query} <-
                   data_result,
                 data = add_tenant(data, new_query),
                 {:ok, data} <-
                   load_through_attributes(
                     data,
                     %{query_ran | calculations: Map.new(calculations_in_query, &{&1.name, &1})},
                     query.domain,
                     opts[:actor],
                     opts[:tracer],
                     opts[:authorize?]
                   ),
                 {:ok, data} <-
                   load_relationships(data, query, opts),
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
              data
              |> Helpers.restrict_field_access(query)
              |> add_tenant(new_query)
              |> attach_fields(opts[:initial_data], initial_query, query, missing_pkeys?)
              |> cleanup_field_auth(query)
              |> add_page(
                query.action,
                count,
                query.sort,
                query,
                new_query,
                opts
              )
              |> add_query(query, opts)
            else
              {:error, %Ash.Query{errors: errors} = query} ->
                {:error, Ash.Error.to_error_class(errors, query: query)}

              {:error,
               %Ash.Error.Forbidden.Placeholder{
                 authorizer: authorizer
               }} ->
                error =
                  Ash.Authorizer.exception(
                    authorizer,
                    :forbidden,
                    query_ran.context[:private][:authorizer_state][authorizer]
                  )

                {:error, Ash.Error.to_error_class(error)}

              {:error, error} ->
                {:error, Ash.Error.to_error_class(error, query: query)}
            end
          after
            if stop? do
              Agent.stop(query.context[:private][:async_limiter])
            end
          end
        end
    end
  end

  defp load_relationships(data, query, opts) do
    lazy? = !!opts[:lazy?]
    reuse_values? = !!opts[:reuse_values?]

    context =
      %{
        actor: opts[:actor],
        tenant: query.tenant,
        authorize?: opts[:authorize?],
        domain: query.domain
      }

    if query.load in [[], nil] do
      {:ok, data}
    else
      case query.action.manual do
        {module, opts} ->
          if module.has_load_relationships?() do
            module.load_relationships(query, data, opts, context, lazy?)
          else
            Ash.Actions.Read.Relationships.load(
              data,
              query,
              lazy?,
              reuse_values?
            )
          end

        _ ->
          Ash.Actions.Read.Relationships.load(
            data,
            query,
            lazy?,
            reuse_values?
          )
      end
    end
  end

  defp do_read(
         %{action: action} = query,
         calculations_at_runtime,
         calculations_in_query,
         source_fields,
         opts
       ) do
    with {:ok, %{valid?: true} = query} <- handle_multitenancy(query),
         query <- add_select_if_none_exists(query),
         pre_authorization_query <- query,
         {:ok, query} <- authorize_query(query, opts),
         {:ok, sort} <-
           add_calc_context_to_sort(
             query.sort,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer],
             query.resource,
             query.domain,
             parent_stack: parent_stack_from_context(query.context),
             first_combination: Enum.at(query.combination_of, 0),
             source_context: query.context
           ),
         query <- %{
           query
           | filter:
               add_calc_context_to_filter(
                 query.filter,
                 opts[:actor],
                 opts[:authorize?],
                 query.tenant,
                 opts[:tracer],
                 query.domain,
                 query.resource,
                 parent_stack: parent_stack_from_context(query.context),
                 source_context: query.context
               ),
             sort: sort
         } do
      case run_before_transaction_hooks(query) do
        {:ok, query} ->
          # Update calculation contexts with any context changes from before_transaction hooks
          query =
            add_calc_context_to_query(
              query,
              opts[:actor],
              opts[:authorize?],
              query.tenant,
              opts[:tracer],
              query.domain,
              expand?: false,
              parent_stack: parent_stack_from_context(query.context),
              source_context: query.context
            )

          calculations_at_runtime =
            Enum.map(calculations_at_runtime, fn calc ->
              add_calc_context(
                calc,
                opts[:actor],
                opts[:authorize?],
                query.tenant,
                opts[:tracer],
                query.domain,
                query.resource,
                source_context: query.context
              )
            end)

          calculations_in_query =
            Enum.map(calculations_in_query, fn calc ->
              add_calc_context(
                calc,
                opts[:actor],
                opts[:authorize?],
                query.tenant,
                opts[:tracer],
                query.domain,
                query.resource,
                source_context: query.context
              )
            end)

          maybe_in_transaction(query, opts, fn notify_callback ->
            with query_before_pagination <- query,
                 {query, calculations_at_runtime, calculations_in_query} <-
                   Ash.Actions.Read.Calculations.deselect_known_forbidden_fields(
                     query,
                     calculations_at_runtime,
                     calculations_in_query,
                     source_fields
                   ),
                 {:ok, data_layer_calculations} <-
                   hydrate_calculations(query, calculations_in_query),
                 {:ok, query} <- hydrate_aggregates(query),
                 {:ok, query} <-
                   hydrate_sort(
                     query,
                     opts[:actor],
                     opts[:authorize?],
                     query.tenant,
                     opts[:tracer],
                     query.domain
                   ),
                 {:ok, query} <-
                   hydrate_combinations(
                     query,
                     opts[:actor],
                     opts[:authorize?],
                     query.tenant,
                     opts[:tracer],
                     query.domain,
                     query.resource,
                     expand?: true,
                     parent_stack: parent_stack_from_context(query.context),
                     source_context: query.context
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
                     query.domain,
                     parent_stack_from_context(query.context),
                     query.context
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
                     query.domain,
                     query.resource,
                     expand?: true,
                     parent_stack: parent_stack_from_context(query.context),
                     source_context: query.context
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
                     query.domain,
                     parent_stack_from_context(query.context),
                     query.context
                   ),
                 query <- Map.put(query, :filter, filter),
                 query <- Ash.Query.unset(query, :calculations),
                 {%{valid?: true} = query, before_notifications} <- run_before_action(query),
                 {:ok, count} <-
                   fetch_count(
                     query,
                     query_before_pagination,
                     relationship_path_filters,
                     opts,
                     true
                   ) do
              ensure_task_stopped(count, fn ->
                with {:ok, query} <- paginate(query, action, opts[:skip_pagination?]),
                     :ok <- validate_combinations(query, calculations_at_runtime, query.load),
                     {:ok, data_layer_query} <-
                       Ash.Query.data_layer_query(query,
                         data_layer_calculations: data_layer_calculations
                       ),
                     {{:ok, results}, query} <-
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
                     {:ok, count} <- maybe_await(count, query.timeout) do
                  notify_callback.(query, before_notifications ++ after_notifications)

                  with {:ok, results} <- run_after_transaction_hooks({:ok, results}, query) do
                    {:ok, results, count, calculations_at_runtime, calculations_in_query, query}
                  end
                else
                  other ->
                    other
                    |> handle_failed_query(notify_callback)
                    |> run_after_transaction_hooks(query)
                end
              end)
            else
              other ->
                other
                |> handle_failed_query(notify_callback)
                |> run_after_transaction_hooks(query)
            end
          end)

        {:error, error} ->
          error_result = {:error, error}
          run_after_transaction_hooks(error_result, query)
      end
    else
      {:ok, query} ->
        {{:error, query}, query}

      {:error, error} ->
        {{:error, error}, query}
    end
  end

  defp ensure_task_stopped(%Task{} = task, fun) do
    fun.()
  after
    Task.shutdown(task, :brutal_kill)
  end

  defp ensure_task_stopped(_, fun), do: fun.()

  defp handle_failed_query(result, notify_callback) do
    case result do
      {%{valid?: false} = query, before_notifications} ->
        notify_callback.(query, before_notifications)
        {:error, query}

      {{:error, %Ash.Query{} = query}, _} ->
        {:error, query}

      {{:error, error}, query} ->
        {:error, Ash.Query.add_error(query, error)}

      {:ok, %Ash.Query{valid?: false} = query} ->
        {:error, query}

      %Ash.Query{} = query ->
        {:error, query}

      {:error, error} ->
        {:error, error}
    end
  end

  defp data_layer_query(
         %{action: action} = query,
         calculations_at_runtime,
         calculations_in_query,
         source_fields,
         opts
       ) do
    initial_query = query

    with {:ok, %{valid?: true} = query} <- handle_multitenancy(query),
         query <- add_select_if_none_exists(query),
         pre_authorization_query <- query,
         {:ok, query} <- authorize_query(query, opts),
         {:ok, sort} <-
           add_calc_context_to_sort(
             query.sort,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer],
             query.resource,
             query.domain,
             parent_stack: parent_stack_from_context(query.context),
             first_combination: Enum.at(query.combination_of, 0),
             source_context: query.context
           ),
         query <- %{
           query
           | filter:
               add_calc_context_to_filter(
                 query.filter,
                 opts[:actor],
                 opts[:authorize?],
                 query.tenant,
                 opts[:tracer],
                 query.domain,
                 query.resource,
                 parent_stack: parent_stack_from_context(query.context),
                 source_context: query.context
               ),
             sort: sort
         },
         query_before_pagination <- query,
         {query, calculations_at_runtime, calculations_in_query} <-
           Ash.Actions.Read.Calculations.deselect_known_forbidden_fields(
             query,
             calculations_at_runtime,
             calculations_in_query,
             source_fields
           ),
         {:ok, data_layer_calculations} <- hydrate_calculations(query, calculations_in_query),
         {:ok, query} <- hydrate_aggregates(query),
         {:ok, query} <-
           hydrate_sort(
             query,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer],
             query.domain
           ),
         {:ok, query} <-
           hydrate_combinations(
             query,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer],
             query.domain,
             query.resource,
             expand?: true,
             parent_stack: parent_stack_from_context(query.context),
             source_context: query.context
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
             query.domain,
             parent_stack_from_context(query.context),
             query.context
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
             query.domain,
             query.resource,
             expand?: true,
             parent_stack: parent_stack_from_context(query.context),
             source_context: query.context
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
             query.domain,
             parent_stack_from_context(query.context),
             query.context
           ),
         query <- Map.put(query, :filter, filter),
         query <- Ash.Query.unset(query, :calculations),
         {%{valid?: true} = query, before_notifications} <- run_before_action(query),
         {:ok, count} <-
           fetch_count(
             query,
             query_before_pagination,
             relationship_path_filters,
             opts,
             false
           ) do
      ensure_task_stopped(count, fn ->
        with {:ok, query} <- paginate(query, action, opts[:skip_pagination?]),
             :ok <- validate_combinations(query, calculations_at_runtime, query.load),
             {:ok, data_layer_query} <-
               Ash.Query.data_layer_query(query, data_layer_calculations: data_layer_calculations) do
          {:ok,
           %{
             query: data_layer_query,
             ash_query: query,
             load: fn query_ran, data ->
               with {:ok, data} <-
                      load_through_attributes(
                        data,
                        %{
                          query_ran
                          | calculations: Map.new(calculations_in_query, &{&1.name, &1})
                        },
                        query.domain,
                        opts[:actor],
                        opts[:tracer],
                        opts[:authorize?]
                      ),
                    {:ok, data} <-
                      load_relationships(data, query, opts),
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
                 data
                 |> Helpers.restrict_field_access(query)
                 |> add_tenant(query)
                 |> attach_fields(nil, initial_query, query, false)
                 |> cleanup_field_auth(query)
                 |> add_page(
                   query.action,
                   count,
                   query.sort,
                   initial_query,
                   query,
                   opts
                 )
               else
                 {:error, %Ash.Query{errors: errors} = query} ->
                   {:error, Ash.Error.to_error_class(errors, query: query)}

                 {:error,
                  %Ash.Error.Forbidden.Placeholder{
                    authorizer: authorizer
                  }} ->
                   error =
                     Ash.Authorizer.exception(
                       authorizer,
                       :forbidden,
                       query_ran.context[:private][:authorizer_state][authorizer]
                     )

                   {:error, Ash.Error.to_error_class(error)}

                 {:error, error} ->
                   {:error, Ash.Error.to_error_class(error, query: query)}
               end
             end,
             run: fn data_layer_query ->
               notify? = !Process.put(:ash_started_transaction?, true)

               with {{:ok, results}, query} <-
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
                    {:ok, results, after_notifications} <- run_after_action(query, results) do
                 notify_or_store(query, before_notifications ++ after_notifications, notify?)

                 {:ok, add_tenant(results, query)}
               else
                 {%{valid?: false} = query, before_notifications} ->
                   notify_or_store(query, before_notifications, notify?)
                   {:error, Ash.Error.to_ash_error(query)}

                 {{:error, %Ash.Query{} = query}, _} ->
                   {:error, Ash.Error.to_ash_error(query)}

                 {{:error, error}, query} ->
                   {:error, Ash.Error.to_ash_error(Ash.Query.add_error(query, error))}

                 {:ok, %Ash.Query{valid?: false} = query} ->
                   {:error, Ash.Error.to_ash_error(query)}

                 %Ash.Query{} = query ->
                   {:error, Ash.Error.to_ash_error(query)}

                 {:error, error} ->
                   {:error, Ash.Error.to_ash_error(error)}
               end
             end,
             count: fn -> count.() end
           }}
        else
          {:ok, query} ->
            {{:error, query}, query}

          {:error, error} ->
            {{:error, error}, query}
        end
      end)
    else
      {:ok, query} ->
        {{:error, query}, query}

      {:error, error} ->
        {{:error, error}, query}
    end
  end

  defp validate_combinations(%{combination_of: []}, _, _) do
    :ok
  end

  defp validate_combinations(query, runtime_calculations, load) do
    case query.combination_of do
      [%{type: type} | _] when type != :base ->
        {:error, "Invalid combinations. The first combination must have type `:base`."}

      combination_of ->
        default_select =
          MapSet.to_list(Ash.Resource.Info.selected_by_default_attribute_names(query.resource))

        fieldsets =
          Enum.map(
            combination_of,
            &Enum.sort(Enum.uniq((&1.select || default_select) ++ Map.keys(&1.calculations)))
          )

        case Enum.uniq(fieldsets) do
          [fieldset] ->
            if requires_pkey_but_not_selecting_it(
                 query,
                 fieldset,
                 default_select,
                 runtime_calculations,
                 load
               ) do
              :ok
            else
              {:error,
               "When runtime calculations, loads, or more attributes in the parent select but not the combination fieldsets are present, all fieldsets must contain the primary key of the resource."}
            end

          _ ->
            {:error,
             """
             Invalid combinations. All fieldsets must be the same, got the following:

             #{inspect(query.combination_of)}
             """}
        end
    end
  end

  defp requires_pkey_but_not_selecting_it(
         query,
         fieldset,
         default_select,
         runtime_calculations,
         load
       ) do
    Enum.all?(query.select || default_select, &(&1 in fieldset)) ||
      (Enum.empty?(runtime_calculations) && Enum.empty?(load)) ||
      Enum.all?(
        Ash.Resource.Info.primary_key(query.resource),
        &Enum.member?(fieldset, &1)
      )
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
                  if Map.has_key?(record.calculations, name) do
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
                  else
                    record
                  end
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
      expr
      |> Ash.Filter.hydrate_refs(%{
        resource: query.resource,
        public?: false,
        first_combination: Enum.at(query.combination_of, 0),
        parent_stack: parent_stack_from_context(query.context)
      })
      |> case do
        {:ok, expr} ->
          expr
          |> Ash.Filter.used_aggregates(:*, true)
          |> Enum.map(fn %Ash.Query.Ref{
                           attribute: aggregate,
                           relationship_path: relationship_path
                         } ->
            %{
              aggregate
              | resource: query.resource,
                relationship_path: relationship_path ++ aggregate.relationship_path,
                related?: Map.get(aggregate, :related?, true)
            }
          end)

        _ ->
          []
      end
    end)
    |> Enum.concat(Map.values(query.aggregates))
  end

  defp source_fields(query, lazy_for_initial_data \\ nil) do
    Enum.flat_map(query.load, fn {name, _} ->
      if lazy_for_initial_data && Ash.Resource.loaded?(lazy_for_initial_data, name, lists: :any) do
        []
      else
        case Ash.Resource.Info.relationship(query.resource, name) do
          %{no_attributes?: true} ->
            []

          %{manual: {module, opts}, source_attribute: source_attribute} ->
            fields =
              module.select(opts)

            [source_attribute | fields]

          %{source_attribute: source_attribute} ->
            [source_attribute]
        end
      end
    end)
  end

  defp clear_async_limiter(%{context: %{private: %{async_limiter: async_limiter}}} = query)
       when is_pid(async_limiter) do
    put_in(query.context.private.async_limiter, nil)
  end

  defp clear_async_limiter(query), do: query

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
    notify? = !Process.put(:ash_started_transaction?, true)

    try do
      cond do
        query.action.transaction? ->
          Ash.DataLayer.transaction(
            [query.resource | query.action.touches_resources],
            fn ->
              func.(&notify_or_store(&1, &2, notify?))
            end,
            query.timeout,
            %{
              type: :read,
              metadata: %{
                query: query,
                resource: query.resource,
                action: query.action.name
              },
              data_layer_context: query.context[:data_layer]
            },
            rollback_on_error?: false
          )
          |> case do
            {:error, :rollback} when not is_nil(query.timeout) ->
              {:error,
               Ash.Error.Invalid.Timeout.exception(
                 timeout: query.timeout,
                 name: fn -> "#{inspect(query.resource)}.#{query.action.name}" end
               )}

            {:error, {:error, error}} ->
              {:error, error}

            {:error, error} ->
              {:error, error}

            {:ok, result} ->
              result
          end

        query.timeout ->
          Ash.ProcessHelpers.task_with_timeout(
            fn ->
              func.(&notify_or_store(&1, &2, notify?))
            end,
            query.resource,
            query.timeout,
            "#{inspect(query.resource)}.#{query.action.name}",
            opts[:tracer]
          )

        true ->
          func.(&notify_or_store(&1, &2, notify?))
      end
    after
      if notify? do
        Process.delete(:ash_started_transaction?)
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
      Ash.Query.select(
        query,
        Ash.Resource.Info.selected_by_default_attribute_names(query.resource)
      )
    end
  end

  defp load(
         [first | _] = initial_data,
         query,
         calculations_at_runtime,
         calculations_in_query,
         missing_pkeys?,
         opts
       ) do
    must_be_reselected =
      query.select
      |> List.wrap()
      |> Kernel.--(Ash.Resource.Info.primary_key(query.resource))

    must_be_reselected =
      if opts[:reuse_values?] && !missing_pkeys? do
        must_be_reselected
        |> Enum.reject(&Ash.Resource.selected?(first, &1, forbidden_means_selected?: true))
      else
        must_be_reselected
      end

    {query, calculations_at_runtime, calculations_in_query} =
      Ash.Actions.Read.Calculations.deselect_known_forbidden_fields(
        query,
        calculations_at_runtime,
        calculations_in_query
      )

    if missing_pkeys? ||
         (Enum.empty?(must_be_reselected) && Enum.empty?(query.aggregates) &&
            Enum.empty?(calculations_in_query)) do
      {:ok, initial_data, 0, calculations_at_runtime, calculations_in_query, query}
    else
      reselect_and_load(
        initial_data,
        query,
        must_be_reselected,
        calculations_in_query,
        calculations_at_runtime,
        opts
      )
    end
  end

  defp reselect_and_load(
         initial_data,
         query,
         must_be_reselected,
         calculations_in_query,
         calculations_at_runtime,
         opts
       ) do
    primary_key = Ash.Resource.Info.primary_key(query.resource)

    if Enum.empty?(primary_key) do
      {:error,
       """
       Cannot reselect fields or perform query/runtime calculations for resources that do not have a primary key using `Ash.load`.
       You must ensure that all necessary fields are present, and use the `reuse_values?` option, and compute calculations when
       the query is run.

       Attempted to reselect #{inspect(must_be_reselected)}.

       Attempted to perform query calculations #{inspect(calculations_in_query)}.

       Attempted to perform runtime calculations #{inspect(calculations_at_runtime)}.
       """}
    else
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
             |> Ash.Query.select([])
             |> Ash.Query.load(calculations_in_query)
             |> Ash.Query.select(must_be_reselected)
             |> Ash.DataLayer.Simple.set_data(initial_data)
             |> Ash.Query.do_filter(filter),
           {:ok, %{valid?: true} = query} <- handle_multitenancy(query),
           {:ok, data_layer_calculations} <-
             hydrate_calculations(
               query,
               calculations_in_query
             ),
           {:ok, query} <- hydrate_aggregates(query),
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
               query.domain,
               parent_stack_from_context(query.context),
               query.context
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
           {{:ok, results}, query} <-
             run_query(
               query,
               data_layer_query,
               %{
                 actor: opts[:actor],
                 tenant: query.tenant,
                 authorize?: opts[:authorize?],
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
            {:ok, result, 0, calculations_at_runtime, calculations_in_query, query}

          {:error, error} ->
            {:error, error}
        end
      end
    end
  end

  defp hydrate_combinations(
         query,
         actor,
         authorize?,
         tenant,
         tracer,
         domain,
         _resource,
         opts
       ) do
    Enum.reduce_while(query.combination_of, {:ok, []}, fn combination, {:ok, acc} ->
      {:ok, sort} =
        add_calc_context_to_sort(
          combination.sort,
          actor,
          authorize?,
          tenant,
          tracer,
          query.resource,
          domain,
          opts
        )

      Enum.reduce_while(combination.calculations, {:ok, %{}}, fn {key, calc}, {:ok, acc} ->
        case hydrate_calculations(query, [calc]) do
          {:ok, [{calc, expression}]} ->
            {:cont,
             {:ok,
              Map.put(acc, key, %{
                calc
                | module: Ash.Resource.Calculation.Expression,
                  opts: [expr: expression]
              })}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
      |> case do
        {:error, error} ->
          {:halt, {:error, error}}

        {:ok, calculations} ->
          {:cont,
           {:ok,
            [
              %{
                combination
                | filter:
                    add_calc_context_to_filter(
                      combination.filter,
                      actor,
                      authorize?,
                      tenant,
                      tracer,
                      domain,
                      query.resource,
                      opts
                    ),
                  calculations:
                    Map.new(calculations, fn {key, val} ->
                      {key,
                       add_calc_context(
                         val,
                         actor,
                         authorize?,
                         tenant,
                         tracer,
                         domain,
                         query.resource,
                         opts
                       )}
                    end),
                  sort: sort
              }
              | acc
            ]}}
      end
    end)
    |> case do
      {:ok, combinations} -> {:ok, %{query | combination_of: Enum.reverse(combinations)}}
      {:error, error} -> {:error, error}
    end
  end

  defp hydrate_sort(query, actor, authorize?, tenant, tracer, domain) do
    [:sort, :distinct, :distinct_sort]
    |> Enum.reject(&(Map.get(query, &1) in [nil, []]))
    |> Enum.reduce({:ok, query}, fn key, {:ok, query} ->
      query
      |> Map.get(key)
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
          case Ash.Query.Calculation.from_resource_calculation(
                 query.resource,
                 resource_calculation,
                 source_context: query.context
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
                  {add_calc_context(
                     field,
                     actor,
                     authorize?,
                     tenant,
                     tracer,
                     domain,
                     query.resource,
                     parent_stack: parent_stack_from_context(query.context),
                     source_context: query.context
                   ), direction}

                field ->
                  {field, direction}
              end
            end)

          {:ok, %{query | key => Enum.reverse(sort)}}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp hydrate_aggregates(query) do
    Enum.reduce_while(query.aggregates, {:ok, %{}}, fn {key, aggregate}, {:ok, aggregates} ->
      aggregate = %{
        aggregate
        | query: %{
            aggregate.query
            | filter:
                Ash.Expr.fill_template(
                  aggregate.query.filter,
                  actor: query.context[:private][:actor],
                  tenant: query.to_tenant,
                  args: %{},
                  context: query.context
                )
          }
      }

      case hydrate_sort(
             aggregate.query,
             query.context[:private][:actor],
             query.context[:private][:authorize?],
             query.tenant,
             query.context[:private][:tracer],
             query.domain
           ) do
        {:ok, agg_query} ->
          aggregate = %{aggregate | query: agg_query}

          case aggregate.field do
            %Ash.Query.Calculation{} = calculation ->
              if calculation.module.has_expression?() and
                   Ash.DataLayer.data_layer_can?(query.resource, :expression_calculation) do
                expression = calculation.module.expression(calculation.opts, calculation.context)

                expression =
                  Ash.Expr.fill_template(
                    expression,
                    actor: query.context[:private][:actor],
                    tenant: query.to_tenant,
                    args: calculation.context.arguments,
                    context: query.context
                  )

                case Ash.Filter.hydrate_refs(expression, %{
                       resource:
                         Ash.Resource.Info.related(query.resource, aggregate.relationship_path),
                       public?: false,
                       parent_stack: parent_stack_from_context(query.context)
                     }) do
                  {:ok, expression} ->
                    new_field = %{
                      calculation
                      | module: Ash.Resource.Calculation.Expression,
                        opts: [expr: expression]
                    }

                    {:cont, {:ok, Map.put(aggregates, key, %{aggregate | field: new_field})}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
              else
                {:cont, {:ok, Map.put(aggregates, key, aggregate)}}
              end

            _other ->
              {:cont, {:ok, Map.put(aggregates, key, aggregate)}}
          end

        {:error, error} ->
          {:error, error}
      end
    end)
    |> case do
      {:ok, aggregates} ->
        {:ok, %{query | aggregates: aggregates}}

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
    if opts[:authorize?] && !Enum.empty?(Ash.Resource.Info.authorizers(query.resource)) do
      case Ash.can(query, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             pre_flight?: false,
             filter_with: opts[:authorize_with] || :filter,
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
        loaded_keys =
          calculations
          |> Enum.filter(fn
            {{:__calc_dep__, _}, _} ->
              false

            {_key, calc} ->
              Ash.Resource.loaded?(initial_data, calc)
          end)
          |> Enum.map(&elem(&1, 0))

        Map.drop(calculations, loaded_keys)
      end)
      |> Map.update!(:aggregates, fn aggregates ->
        loaded_keys =
          aggregates
          |> Enum.filter(fn {_key, calc} ->
            Ash.Resource.loaded?(initial_data, calc)
          end)
          |> Enum.map(&elem(&1, 0))

        Map.drop(aggregates, loaded_keys)
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
      Ash.Type.can_load?(type, constraints) && (is_nil(query.select) || name in query.select)
    end)
    |> Enum.map(& &1.name)
    |> Enum.reduce(query.load_through, fn name, load_through ->
      Map.update(load_through, :attribute, %{name => []}, &Map.put_new(&1, name, []))
    end)
    |> Enum.reduce_while({:ok, results}, fn
      {:calculation, load_through}, {:ok, results} ->
        load_through
        |> Map.take(Map.keys(query.calculations))
        |> Enum.reject(fn {_, v} -> is_nil(v) end)
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
            {:array, type} ->
              Enum.reduce_while(values, {:ok, []}, fn list, {:ok, acc} ->
                case Ash.Type.load(
                       type,
                       list,
                       load_statement,
                       calculation.constraints[:items] || [],
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
        |> Enum.reject(fn {_, v} -> is_nil(v) end)
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
            {:array, type} ->
              Enum.reduce_while(values, {:ok, []}, fn list, {:ok, acc} ->
                case Ash.Type.load(
                       type,
                       list,
                       load_statement,
                       attribute.constraints[:items] || [],
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
  def add_calc_context_to_filter(
        filter,
        actor,
        authorize?,
        tenant,
        tracer,
        domain \\ nil,
        resource \\ nil,
        opts \\ []
      ) do
    # backward compatibility fix
    # really need to stop these from being called elsewhere
    {domain, resource, opts} =
      if is_list(domain) and resource == nil and opts == [] do
        case filter do
          %Ash.Filter{resource: resource} ->
            {nil, resource, opts}

          _ ->
            {nil, nil, []}
        end
      else
        {domain, resource, opts}
      end

    Ash.Filter.map(filter, fn
      %Ash.Query.Parent{} = parent ->
        if List.wrap(opts[:parent_stack]) != [] do
          %{
            parent
            | expr:
                add_calc_context_to_filter(
                  parent.expr,
                  actor,
                  authorize?,
                  tenant,
                  tracer,
                  domain,
                  hd(opts[:parent_stack]),
                  Keyword.update!(opts, :parent_stack, &tl/1)
                )
          }
        else
          parent
        end

      %Ash.Query.Exists{} = exists ->
        if List.wrap(opts[:parent_stack]) != [] do
          %{
            exists
            | expr:
                add_calc_context_to_filter(
                  exists.expr,
                  actor,
                  authorize?,
                  tenant,
                  tracer,
                  domain,
                  Ash.Resource.Info.related(resource, exists.path),
                  Keyword.update(opts, :parent_stack, [resource], &[resource, &1])
                )
          }
        else
          exists
        end

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
        calc = add_calc_context(calc, actor, authorize?, tenant, tracer, domain, resource, opts)

        if Keyword.get(opts, :expand?, false) && calc.module.has_expression?() do
          expr =
            case calc.module.expression(calc.opts, calc.context) do
              %Ash.Query.Function.Type{} = expr ->
                expr

              %Ash.Query.Call{name: :type} = expr ->
                expr

              expr ->
                if calc.type do
                  {:ok, expr} =
                    Ash.Query.Function.Type.new([expr, calc.type, calc.constraints])

                  expr
                else
                  expr
                end
            end

          {:ok, expr} =
            Ash.Filter.hydrate_refs(
              expr,
              %{
                resource: ref.resource,
                parent_stack: opts[:parent_stack] || [],
                public?: false
              }
            )

          expr =
            Ash.Expr.fill_template(
              expr,
              actor: actor,
              tenant: tenant,
              args: calc.context.arguments,
              context: opts[:source_context] || %{}
            )

          expr =
            add_calc_context_to_filter(
              expr,
              actor,
              authorize?,
              tenant,
              tracer,
              domain,
              ref.resource,
              opts
            )

          expanded_calc = %Ash.Query.Calculation{
            name: calc.name,
            module: Ash.Resource.Calculation.Expression,
            opts: [expr: expr],
            type: calc.type,
            constraints: calc.constraints,
            filterable?: calc.filterable?,
            sortable?: calc.sortable?,
            sensitive?: calc.sensitive?,
            load: calc.load,
            select: calc.select,
            context: calc.context
          }

          # Return a Ref at the relationship path with the expanded calculation
          %{ref | attribute: expanded_calc, relationship_path: relationship_path}
        else
          %{ref | attribute: calc}
        end

      %Ash.Query.Ref{attribute: %Ash.Query.Aggregate{} = agg} = ref ->
        %{
          ref
          | attribute:
              add_calc_context(agg, actor, authorize?, tenant, tracer, domain, ref.resource, opts)
        }

      other ->
        other
    end)
  end

  defp add_calc_context_to_sort(empty, _, _, _, _, _, _, _opts) when empty in [[], nil],
    do: {:ok, empty}

  defp add_calc_context_to_sort(sort, actor, authorize?, tenant, tracer, resource, domain, opts) do
    sort
    |> Enum.reduce_while({:ok, []}, fn
      {%struct{} = calc, order}, {:ok, acc}
      when struct in [
             Ash.Query.Calculation,
             Ash.Aggregate.Calculation,
             Ash.Resource.Calculation,
             Ash.Resource.Aggregate
           ] ->
        calc = add_calc_context(calc, actor, authorize?, tenant, tracer, domain, resource, opts)

        if should_expand_expression?(struct, calc, opts) do
          case expand_expression(calc, resource, opts[:parent_stack], opts[:first_combination]) do
            {:ok, expr} ->
              args =
                case calc do
                  %Ash.Query.Calculation{context: %{arguments: arguments}} -> arguments
                  _ -> %{}
                end

              expr =
                Ash.Expr.fill_template(
                  expr,
                  actor: actor,
                  tenant: tenant,
                  args: args,
                  context: opts[:source_context]
                )

              expr =
                add_calc_context_to_filter(
                  expr,
                  actor,
                  authorize?,
                  tenant,
                  tracer,
                  domain,
                  resource,
                  opts
                )

              module = Ash.Resource.Calculation.Expression
              calc = %{calc | module: module, opts: [expr: expr]}
              {:cont, {:ok, [{calc, order} | acc]}}

            error ->
              {:halt, error}
          end
        else
          {:cont, {:ok, [{calc, order} | acc]}}
        end

      {calc, order}, {:ok, acc} ->
        {:cont, {:ok, [{calc, order} | acc]}}
    end)
    |> then(fn
      {:ok, sort} -> {:ok, Enum.reverse(sort)}
      result -> result
    end)
  end

  defp should_expand_expression?(struct, calc, opts) do
    struct == Ash.Query.Calculation &&
      Keyword.get(opts, :expand?, true) &&
      calc.module.has_expression?()
  end

  defp expand_expression(calc, resource, parent_stack, first_combination) do
    calc.module.expression(calc.opts, calc.context)
    |> case do
      %Ash.Query.Function.Type{} = expr ->
        expr

      %Ash.Query.Call{name: :type} = expr ->
        expr

      expr ->
        if calc.type do
          {:ok, expr} =
            Ash.Query.Function.Type.new([expr, calc.type, calc.constraints])

          expr
        else
          expr
        end
    end
    |> Ash.Filter.hydrate_refs(%{
      resource: resource,
      public?: false,
      parent_stack: parent_stack,
      first_combination: first_combination
    })
  end

  @doc false
  def handle_multitenancy(query) do
    action_multitenancy =
      get_shared_multitenancy(query) || get_action(query.resource, query.action).multitenancy

    case action_multitenancy do
      :enforce ->
        query = handle_attribute_multitenancy(query)

        with :ok <- validate_multitenancy(query) do
          {:ok, query}
        end

      :allow_global ->
        {:ok, handle_attribute_multitenancy(query)}

      :bypass ->
        {:ok, query}

      :bypass_all ->
        query = Ash.Query.set_context(query, %{shared: %{multitenancy: :bypass_all}})

        {:ok, query}
    end
    |> case do
      {:ok, query} -> handle_aggregate_multitenancy(query)
      other -> other
    end
  end

  defp handle_attribute_multitenancy(query) do
    if query.tenant && Ash.Resource.Info.multitenancy_strategy(query.resource) == :attribute do
      multitenancy_attribute = Ash.Resource.Info.multitenancy_attribute(query.resource)

      if multitenancy_attribute do
        {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(query.resource)
        attribute_value = apply(m, f, [query.to_tenant | a])
        Ash.Query.filter(query, ^ref(multitenancy_attribute) == ^attribute_value)
      else
        query
      end
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

  @doc false
  def for_read(query, action, initial_data, args \\ %{}, opts) do
    if query.__validated_for_action__ == action.name do
      query
    else
      load = query.load
      calculations = query.calculations
      aggregates = query.aggregates
      load_through = query.load_through
      query = Ash.Query.for_read(query, action.name, args, opts)

      if strip_load?(initial_data) do
        %{
          query
          | load: load,
            aggregates: aggregates,
            calculations: calculations,
            load_through: load_through
        }
      else
        if prefer_existing_loads?(query) do
          %{
            query
            | load: Keyword.merge(load, query.load),
              aggregates: Map.merge(aggregates, query.aggregates),
              calculations: Map.merge(calculations, query.calculations),
              load_through: %{
                calculation:
                  Map.merge(
                    load_through[:calculation] || %{},
                    query.load_through[:calculation] || %{}
                  ),
                attribute:
                  Map.merge(
                    load_through[:attribute] || %{},
                    query.load_through[:attribute] || %{}
                  )
              }
          }
        else
          %{
            query
            | load: Keyword.merge(query.load, load),
              aggregates: Map.merge(query.aggregates, aggregates),
              calculations: Map.merge(query.calculations, calculations),
              load_through: %{
                calculation:
                  Map.merge(
                    query.load_through[:calculation] || %{},
                    load_through[:calculation] || %{}
                  ),
                attribute:
                  Map.merge(
                    query.load_through[:attribute] || %{},
                    load_through[:attribute] || %{}
                  )
              }
          }
        end
      end
    end
  end

  # The function `keep_read_action_loads_when_loading?` always returns a constant value
  # because its a compile attr
  # So dialyzer always complains that `!false` can never be true
  @dialyzer {:nowarn_function, strip_load?: 1}
  defp strip_load?(initial_data) do
    initial_data && !Ash.Actions.Helpers.keep_read_action_loads_when_loading?()
  end

  @dialyzer {:nowarn_function, prefer_existing_loads?: 1}
  defp prefer_existing_loads?(query) do
    query.context[:loading_relationships?] &&
      !Ash.Actions.Helpers.keep_read_action_loads_when_loading?()
  end

  defp validate_multitenancy(query) do
    if is_nil(Ash.Resource.Info.multitenancy_strategy(query.resource)) ||
         Ash.Resource.Info.multitenancy_global?(query.resource) || query.tenant do
      :ok
    else
      {:error, Ash.Error.Invalid.TenantRequired.exception(resource: query.resource)}
    end
  end

  defp handle_aggregate_multitenancy(query) do
    Enum.reduce_while(query.aggregates, {:ok, %{}}, fn {key, aggregate}, {:ok, acc} ->
      case handle_multitenancy(
             Ash.Query.set_tenant(aggregate.query, aggregate.query.tenant || query.tenant)
           ) do
        {:ok, %{valid?: true} = query} ->
          {:cont, {:ok, Map.put(acc, key, %{aggregate | query: query})}}

        {:ok, query} ->
          {:halt, {:error, Ash.Error.set_path(query.errors, aggregate.name)}}

        {:error, error} ->
          {:halt, {:error, Ash.Error.set_path(error, aggregate.name)}}
      end
    end)
    |> case do
      {:ok, aggregates} -> {:ok, %{query | aggregates: aggregates}}
      {:error, error} -> {:error, error}
    end
  end

  defp add_tenant(data, query) do
    if Ash.Resource.Info.multitenancy_strategy(query.resource) do
      Enum.map(data, fn item ->
        %{item | __metadata__: Map.put_new(item.__metadata__, :tenant, query.tenant)}
      end)
    else
      data
    end
  end

  defp add_query(result, query, opts) do
    if opts[:return_query?] do
      query = clear_async_limiter(query)
      {:ok, result, query}
    else
      {:ok, result}
    end
  end

  @doc false
  def add_page(data, action, count, sort, original_query, new_query, opts) do
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
        to_page(data, action, count, sort, original_query, new_query, opts)

      true ->
        data
    end
  end

  @doc false
  def to_page(data, action, count, sort, original_query, new_query, opts) do
    # prevent leakage of stale pid as we stop it at the end of reading
    original_query =
      clear_async_limiter(original_query)

    count = count || new_query.context[:full_count]
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
          case Application.get_env(:ash, :default_page_type, :offset) do
            :keyset ->
              Ash.Page.Keyset.new(data, count, sort, original_query, more?, opts)

            :offset ->
              Ash.Page.Offset.new(data, count, original_query, more?, opts)
          end

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
        if Ash.Resource.Info.primary_key_simple_equality?(original_query.resource) do
          key_fun =
            case Ash.Resource.Info.primary_key(original_query.resource) do
              [field] -> &{Map.get(&1, field), &1}
              fields -> &{Map.take(&1, fields), &1}
            end

          data_with_selected = Map.new(data_with_selected, key_fun)

          Enum.map(data, fn record ->
            case Map.fetch(data_with_selected, elem(key_fun.(record), 0)) do
              {:ok, match} ->
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

              :error ->
                Ash.Resource.put_metadata(record, :private, %{missing_from_data_layer: true})
            end
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

  @doc false
  def add_calc_context_to_query(query, actor, authorize?, tenant, tracer, domain, opts) do
    {:ok, sort} =
      add_calc_context_to_sort(
        query.sort,
        actor,
        authorize?,
        tenant,
        tracer,
        query.resource,
        domain,
        Keyword.put(opts, :first_combination, Enum.at(query.combination_of, 0))
      )

    %{
      query
      | sort: sort,
        aggregates:
          Map.new(query.aggregates, fn {key, agg} ->
            {key,
             add_calc_context(
               agg,
               actor,
               agg.authorize? && authorize?,
               tenant,
               tracer,
               domain,
               query.resource,
               opts
             )}
          end),
        calculations:
          Map.new(query.calculations, fn {key, calc} ->
            {key,
             add_calc_context(
               calc,
               actor,
               authorize?,
               tenant,
               tracer,
               domain,
               query.resource,
               opts
             )}
          end),
        filter:
          add_calc_context_to_filter(
            query.filter,
            actor,
            authorize?,
            tenant,
            tracer,
            domain,
            query.resource,
            opts
          )
    }
    |> add_calc_context_to_loads(actor, authorize?, tenant, tracer, domain)
  end

  defp add_calc_context_to_loads(query, actor, authorize?, tenant, tracer, domain) do
    %{
      query
      | load:
          Keyword.new(query.load, fn {key, related_query} ->
            case related_query do
              %Ash.Query{} = related_query ->
                {key,
                 add_calc_context_to_loads(
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
                  |> add_calc_context_to_loads(actor, authorize?, tenant, tracer, domain)

                {key, load}
            end
          end)
    }
  end

  def add_calc_context(
        item,
        actor,
        authorize?,
        tenant,
        tracer,
        domain,
        resource \\ nil,
        opts \\ []
      )

  @doc false
  def add_calc_context(
        %Ash.Query.Aggregate{} = agg,
        actor,
        authorize?,
        tenant,
        tracer,
        domain,
        resource,
        opts
      ) do
    query =
      if is_nil(agg.query.__validated_for_action__) && agg.relationship_path != [] do
        read_action =
          agg.read_action || Ash.Resource.Info.primary_action!(agg.query.resource, :read).name

        domain =
          if agg.query.domain do
            agg.query.domain
          else
            Ash.Domain.Info.related_domain(agg.resource, agg.relationship_path, domain)
          end

        agg.query
        |> Ash.Query.set_context(%{private: %{require_actor?: false}})
        |> Ash.Query.for_read(read_action, %{},
          domain: domain,
          actor: actor,
          tenant: tenant,
          authorize?: agg.authorize? && authorize?
        )
      else
        Ash.Query.set_context(agg.query, %{
          private: %{
            authorize?: agg.authorize? && authorize?,
            actor: actor
          }
        })
      end

    authorize? =
      case agg.name do
        {:__calc_dep__, _} ->
          true

        _ ->
          authorize?
      end

    opts =
      if resource do
        Keyword.update(opts, :parent_stack, [resource], &[resource, &1])
      else
        opts
      end

    field =
      if is_atom(agg.field) do
        Ash.Resource.Info.field(query.resource, agg.field)
      else
        agg.field
      end

    field =
      case field do
        %struct{} = nested
        when struct in [
               Ash.Query.Aggregate,
               Ash.Query.Calculation,
               Ash.Resource.Aggregate,
               Ash.Resource.Calculation
             ] ->
          add_calc_context(
            nested,
            actor,
            authorize?,
            tenant,
            tracer,
            domain,
            query.resource,
            opts
          )

        _ ->
          agg.field
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
        query: add_calc_context_to_query(query, actor, authorize?, tenant, tracer, domain, opts),
        field: field,
        join_filters:
          Map.new(agg.join_filters, fn {key, filter} ->
            {key,
             add_calc_context_to_filter(
               filter,
               actor,
               authorize?,
               tenant,
               tracer,
               domain,
               query.resource,
               opts
             )}
          end)
    }
  end

  def add_calc_context(
        %Ash.Resource.Aggregate{} = agg,
        actor,
        authorize?,
        tenant,
        tracer,
        domain,
        resource,
        opts
      ) do
    agg_opts = [
      actor: actor,
      authorize?: authorize?,
      tenant: tenant,
      tracer: tracer,
      domain: domain
    ]

    {:ok, aggregate} = resource_aggregate_to_query_aggregate(resource, agg, agg_opts)

    add_calc_context(aggregate, actor, authorize?, tenant, tracer, domain, resource, opts)
  end

  def add_calc_context(
        %Ash.Resource.Calculation{} = calc,
        actor,
        authorize?,
        tenant,
        tracer,
        domain,
        resource,
        opts
      ) do
    {:ok, calc} =
      Ash.Query.Calculation.from_resource_calculation(resource, calc.name,
        source_context: opts[:source_context]
      )

    add_calc_context(calc, actor, authorize?, tenant, tracer, domain, resource, opts)
  end

  def add_calc_context(calc, actor, authorize?, tenant, tracer, _domain, _resource, opts) do
    authorize? =
      case calc.name do
        {:__calc_dep__, _} ->
          true

        _ ->
          authorize?
      end

    %{
      calc
      | context: %{
          calc.context
          | actor: actor,
            authorize?: authorize?,
            tenant: tenant,
            tracer: tracer,
            source_context: opts[:source_context] || %{}
        }
    }
  end

  @doc false
  def update_aggregate_filters(
        filter,
        resource,
        authorize?,
        relationship_path_filters,
        actor,
        tenant,
        tracer,
        domain,
        parent_stack,
        source_context
      ) do
    if authorize? do
      Filter.update_aggregates(
        filter,
        resource,
        fn aggregate, ref, parent_stack ->
          if aggregate.authorize? do
            authorize_aggregate(
              aggregate,
              relationship_path_filters,
              actor,
              authorize?,
              tenant,
              tracer,
              domain,
              resource,
              ref.relationship_path,
              parent_stack,
              source_context
            )
          else
            aggregate
          end
        end,
        [],
        Enum.map(parent_stack, &{&1, []})
      )
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
    if aggregate_relationship_path == [] do
      current_join_filters
    else
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
  end

  defp maybe_await(%Task{} = task, timeout) do
    case Task.await(task, timeout) do
      {:__exception__, e, stacktrace} ->
        reraise e, stacktrace

      other ->
        other
    end
  end

  defp maybe_await(other, _timeout), do: other

  defp fetch_count(
         %{context: %{full_count: full_count}},
         _,
         _,
         _,
         return?
       )
       when not is_nil(full_count) do
    if return? do
      {:ok, {:ok, full_count}}
    else
      {:ok, fn -> {:ok, full_count} end}
    end
  end

  defp fetch_count(
         %{action: %{manual: {_mod, _opts}}},
         _,
         _,
         _,
         return?
       ) do
    if return? do
      {:ok, {:ok, nil}}
    else
      {:ok, fn -> {:ok, nil} end}
    end
  end

  defp fetch_count(
         %{action: action, resource: resource, page: page} = query,
         query_before_pagination,
         relationship_path_filters,
         opts,
         return?
       ) do
    needs_count? =
      action.pagination && page &&
        (page[:count] == true ||
           (page[:count] != false and action.pagination.countable == :by_default))

    cond do
      query.filter && query.filter.expression == false ->
        result = if needs_count?, do: 0, else: nil

        if return? do
          {:ok, {:ok, result}}
        else
          {:ok, fn -> {:ok, result} end}
        end

      Map.has_key?(query.context, :accessing_from) and needs_count? ->
        if return? do
          # Relationship count is fetched by the parent using aggregates, just return nil here
          {:ok, {:ok, nil}}
        else
          {:ok, fn -> {:ok, nil} end}
        end

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
          if return? do
            if Ash.DataLayer.in_transaction?(resource) ||
                 !Ash.DataLayer.can?(:async_engine, resource) ||
                 Application.get_env(:ash, :disable_async?) do
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
          else
            {:ok, fn -> do_fetch_count(query, data_layer_query) end}
          end
        end

      true ->
        if return? do
          {:ok, {:ok, nil}}
        else
          {:ok, fn -> {:ok, nil} end}
        end
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
      subject: query,
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

      state =
        cond do
          is_struct(state) ->
            if Map.has_key?(state, :subject) && !state.subject do
              Map.put(state, :subject, query)
            else
              state
            end

          is_map(state) && !Map.has_key?(state, :subject) ->
            Map.put(state, :subject, query)

          true ->
            state
        end

      context = Map.take(data, Ash.Authorizer.strict_check_context(authorizer, data))

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
    case Ash.Query.Aggregate.new(query.resource, :count, :count, tenant: query.tenant) do
      {:ok, aggregate} ->
        Ash.DataLayer.run_aggregate_query(data_layer_query, [aggregate], query.resource)

      {:error, error} ->
        {:error, error}
    end
  end

  def page_opts(action, page_opts, relationship?) do
    cond do
      action.type != :read ->
        nil

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

      page_opts[:count] == true && !action.pagination.countable ->
        {:error,
         NonCountableAction.exception(resource: starting_query.resource, action: action.name)}

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
        Ash.Query.sort(
          query,
          pagination.stable_sort || Ash.Resource.Info.primary_key(query.resource)
        )
      end

    paginated =
      case pagination_type(page_opts, query.action.pagination) do
        :keyset -> keyset_pagination(query, pagination, page_opts)
        :offset -> limit_offset_pagination(query, pagination, page_opts)
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

  defp pagination_type(_page_opts, pagination) when pagination in [nil, false], do: nil

  defp pagination_type(page_opts, pagination) do
    cond do
      !page_opts ->
        nil

      page_opts[:before] || page_opts[:after] ->
        :keyset

      page_opts[:offset] ->
        :offset

      pagination.keyset? ->
        :keyset

      pagination.offset? ->
        :offset

      true ->
        nil
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

  defp load_and_select_sort(query, page_opts) do
    if Application.get_env(:ash, :show_keysets_for_all_actions?, true) do
      # in 4.0 remove this branch
      show_keyset? =
        query.resource
        |> Ash.Resource.Info.actions()
        |> Enum.any?(&match?(%{pagination: %{keyset?: true}}, &1))

      if show_keyset? do
        query.sort
        |> Enum.map(&elem(&1, 0))
        |> then(fn load ->
          Ash.Query.load(query, load)
        end)
      else
        query
      end
    else
      case pagination_type(page_opts, query.action.pagination) do
        :keyset ->
          query.sort
          |> Enum.map(&elem(&1, 0))
          |> then(fn load ->
            Ash.Query.load(query, load)
          end)

        _ ->
          query
      end
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
    cond do
      query.limit == 0 ->
        {:ok, []}

      query.filter && query.filter.expression == false ->
        Logger.debug("""
        #{inspect(query.resource)}.#{query.action.name}: skipped query run due to filter being false"
        """)

        {:ok, []}

      true ->
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
    |> then(&{&1, query})
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
    cond do
      query.limit == 0 ->
        {:ok, []}

      query.filter && query.filter.expression == false ->
        Logger.debug("""
        #{inspect(query.resource)}.#{query.action.name}: skipped query run due to filter being false"
        """)

        {:ok, []}

      true ->
        data_layer_query
        |> Ash.DataLayer.run_query_with_lateral_join(
          root_data,
          destination_resource,
          path
        )
        |> Helpers.select(query)
        |> Helpers.load_runtime_types(query, load_attributes?)
    end
    |> then(&{&1, query})
  end

  defp run_query(
         %{action: %{manual: {mod, opts}}} = query,
         data_layer_query,
         context,
         load_attributes?
       ) do
    {result, query} =
      query
      |> mod.read(data_layer_query, opts, context)
      |> case do
        {:ok, result, extra_info} ->
          query =
            if extra_info[:full_count] do
              Ash.Query.set_context(query, %{full_count: extra_info[:full_count]})
            else
              query
            end

          {{:ok, result}, query}

        other ->
          {other, query}
      end

    result
    |> validate_manual_action_return_result!(query.resource, query.action)
    |> Helpers.select(query)
    |> Helpers.load_runtime_types(query, load_attributes?)
    |> then(&{&1, query})
  end

  defp run_query(
         %{resource: resource} = query,
         data_layer_query,
         _context,
         load_attributes?
       ) do
    cond do
      query.limit == 0 ->
        {:ok, []}

      query.filter && query.filter.expression == false ->
        Logger.debug("""
        #{inspect(query.resource)}.#{query.action.name}: skipped query run due to filter being false"
        """)

        {:ok, []}

      true ->
        data_layer_query
        |> Ash.DataLayer.run_query(resource)
        |> Helpers.rollback_if_in_transaction(query.resource, query)
        |> Helpers.select(query)
        |> Helpers.load_runtime_types(query, load_attributes?)
    end
    |> then(&{&1, query})
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
            actor: calculation.context.actor,
            tenant: query.to_tenant,
            args: calculation.context.arguments,
            context: calculation.context.source_context
          )

        case Ash.Filter.hydrate_refs(expression, %{
               resource: query.resource,
               public?: false,
               first_combination: Enum.at(query.combination_of, 0),
               parent_stack: parent_stack_from_context(query.context)
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

  @doc false
  def parent_stack_from_context(
        %{
          data_layer: %{lateral_join_source: {_, [{%{resource: resource}, _, _, _} | _]}}
        } = context
      ) do
    [resource] ++ List.wrap(context[:parent_stack])
  end

  def parent_stack_from_context(context) do
    List.wrap(context[:parent_stack])
  end

  defp authorize_calculation_expressions(
         hydrated_calculations,
         resource,
         authorize?,
         relationship_path_filters,
         actor,
         tenant,
         tracer,
         domain,
         parent_stack,
         source_context
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
         domain,
         parent_stack,
         source_context
       )}
    end)
  end

  defp authorize_loaded_aggregates(
         query,
         path_filters,
         actor,
         authorize?,
         tenant,
         tracer
       ) do
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
            query.domain,
            query.resource,
            [],
            parent_stack_from_context(query.context),
            query.context
          )
        else
          add_calc_context(
            aggregate,
            actor,
            authorize?,
            tenant,
            tracer,
            query.domain,
            query.resource,
            parent_stack: parent_stack_from_context(query.context),
            source_context: query.context
          )
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
              query.domain,
              query.resource,
              [],
              parent_stack_from_context(query.context),
              query.context
            )
          else
            add_calc_context(
              aggregate,
              actor,
              authorize?,
              tenant,
              tracer,
              query.domain,
              query.resource,
              parent_stack: parent_stack_from_context(query.context),
              source_context: query.context
            )
          end

        {:cont, {:ok, [{new_agg, direction} | sort]}}

      {%Ash.Query.Calculation{
         module: Ash.Resource.Calculation.Expression,
         opts: [expr: expression]
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
            query.domain,
            parent_stack_from_context(query.context),
            query.context
          )

        new_calc = %{calc | opts: [expr: new_expr]}

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

    {related_resource, aggregate_opts} =
      if Map.get(resource_aggregate, :related?, true) do
        related_resource =
          Ash.Resource.Info.related(resource, resource_aggregate.relationship_path)

        opts = [path: resource_aggregate.relationship_path]
        {related_resource, opts}
      else
        related_resource = resource_aggregate.resource
        opts = [resource: related_resource]
        {related_resource, opts}
      end

    read_action =
      resource_aggregate.read_action ||
        Ash.Resource.Info.primary_action!(related_resource, :read).name

    opts = query.context[:private] |> Map.take([:actor, :authorize?, :tenant]) |> Map.to_list()

    with %{valid?: true} = aggregate_query <-
           Ash.Query.for_read(related_resource, read_action, %{}, opts),
         %{valid?: true} = aggregate_query <-
           Ash.Query.Aggregate.build_query(
             aggregate_query,
             resource,
             filter: resource_aggregate.filter,
             sort: resource_aggregate.sort
           ),
         {:ok, query_aggregate} <-
           (
             full_opts =
               [
                 agg_name: resource_aggregate.name,
                 query: aggregate_query,
                 field: resource_aggregate.field,
                 default: resource_aggregate.default,
                 filterable?: resource_aggregate.filterable?,
                 type: resource_aggregate.type,
                 constraints: resource_aggregate.constraints,
                 implementation: resource_aggregate.implementation,
                 include_nil?: resource_aggregate.include_nil?,
                 uniq?: resource_aggregate.uniq?,
                 read_action: read_action,
                 authorize?: resource_aggregate.authorize?,
                 join_filters:
                   Map.new(resource_aggregate.join_filters, &{&1.relationship_path, &1.filter})
               ] ++ aggregate_opts

             Ash.Query.Aggregate.new(
               resource,
               resource_aggregate.name,
               resource_aggregate.kind,
               full_opts
             )
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
         resource,
         ref_path,
         parent_stack,
         source_context
       ) do
    aggregate =
      add_calc_context(aggregate, actor, authorize?, tenant, tracer, domain, resource,
        parent_stack: parent_stack,
        source_context: source_context
      )

    additional_filter =
      if Map.get(aggregate, :related?, true) do
        last_relationship = last_relationship(aggregate.resource, aggregate.relationship_path)

        case Map.fetch(
               path_filters,
               {last_relationship.source, last_relationship.name, aggregate.query.action.name}
             ) do
          :error ->
            true

          {:ok, filter} ->
            filter
        end
      else
        case Map.fetch(
               path_filters,
               {aggregate.query.resource, aggregate.query.action.name}
             ) do
          :error ->
            true

          {:ok, filter} ->
            filter
        end
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
             domain,
             [resource | parent_stack],
             source_context
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
             ref_path,
             parent_stack,
             source_context
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
         _ref_path,
         _parent_stack,
         _source_context
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
         ref_path,
         parent_stack,
         source_context
       ) do
    calc =
      add_calc_context(field, actor, authorize?, tenant, tracer, domain, agg.resource,
        parent_stack: parent_stack,
        source_context: source_context
      )

    related_resource = Ash.Resource.Info.related(agg.resource, agg.relationship_path)

    if calc.module.has_expression?() do
      expr =
        case calc.module.expression(calc.opts, calc.context) do
          %Ash.Query.Function.Type{} = expr ->
            expr

          %Ash.Query.Call{name: :type} = expr ->
            expr

          expr ->
            if calc.type do
              {:ok, expr} =
                Ash.Query.Function.Type.new([expr, calc.type, calc.constraints])

              expr
            else
              expr
            end
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
          domain,
          agg.query.resource,
          source_context: agg.query.context
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
         ref_path,
         parent_stack,
         source_context
       ) do
    field =
      add_calc_context(field, actor, authorize?, tenant, tracer, domain, agg.resource,
        parent_stack: parent_stack,
        source_context: source_context
      )

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
         agg.resource,
         ref_path,
         [agg.resource | parent_stack],
         source_context
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
         ref_path,
         parent_stack,
         source_context
       )
       when is_atom(aggregate.field) do
    related_resource = Ash.Resource.Info.related(aggregate.resource, aggregate.relationship_path)

    case Ash.Resource.Info.field(related_resource, aggregate.field) do
      %Ash.Resource.Calculation{} = resource_calculation ->
        case Ash.Query.Calculation.from_resource_calculation(
               aggregate.resource,
               resource_calculation,
               source_context: aggregate.context[:source_context] || %{}
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
              ref_path,
              parent_stack,
              source_context
            )

          {:error, error} ->
            {:error, error}
        end

      %Ash.Resource.Aggregate{} = resource_aggregate ->
        opts = [
          actor: actor,
          authorize?: authorize?,
          tenant: tenant,
          tracer: tracer,
          domain: domain
        ]

        case resource_aggregate_to_query_aggregate(related_resource, resource_aggregate, opts) do
          {:ok, query_aggregate} ->
            aggregate_field_with_related_filters(
              %{aggregate | field: query_aggregate},
              path_filters,
              actor,
              authorize?,
              tenant,
              tracer,
              domain,
              ref_path,
              parent_stack,
              source_context
            )

          {:error, error} ->
            {:error, error}
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
         _ref_path,
         _parent_stack,
         _source_context
       )
       when is_atom(aggregate.field) do
    {:ok, aggregate.field}
  end

  defp resource_aggregate_to_query_aggregate(resource, resource_aggregate, opts) do
    {agg_related_resource, aggregate_opts} =
      if Map.get(resource_aggregate, :related?, true) do
        agg_related_resource =
          Ash.Resource.Info.related(resource, resource_aggregate.relationship_path)

        opts_for_aggregate = [path: resource_aggregate.relationship_path]
        {agg_related_resource, opts_for_aggregate}
      else
        agg_related_resource = resource_aggregate.resource
        opts_for_aggregate = [resource: agg_related_resource]
        {agg_related_resource, opts_for_aggregate}
      end

    read_action =
      resource_aggregate.read_action ||
        Ash.Resource.Info.primary_action!(agg_related_resource, :read).name

    with %{valid?: true} = aggregate_query <-
           Ash.Query.for_read(agg_related_resource, read_action, %{}, opts),
         %{valid?: true} = aggregate_query <-
           Ash.Query.Aggregate.build_query(
             aggregate_query,
             resource,
             filter: resource_aggregate.filter,
             sort: resource_aggregate.sort
           ) do
      Ash.Query.Aggregate.new(
        resource,
        resource_aggregate.name,
        resource_aggregate.kind,
        [
          agg_name: resource_aggregate.name,
          query: aggregate_query,
          field: resource_aggregate.field,
          default: resource_aggregate.default,
          filterable?: resource_aggregate.filterable?,
          type: resource_aggregate.type,
          constraints: resource_aggregate.constraints,
          include_nil?: resource_aggregate.include_nil?,
          implementation: resource_aggregate.implementation,
          uniq?: resource_aggregate.uniq?,
          read_action: read_action,
          authorize?: resource_aggregate.authorize?,
          join_filters:
            Map.new(resource_aggregate.join_filters, &{&1.relationship_path, &1.filter})
        ] ++ aggregate_opts
      )
    end
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
           %Ash.Query.Exists{
             at_path: at_path,
             path: exists_path,
             expr: exists_expr,
             related?: related?,
             resource: unrelated_resource
           } = exists ->
             if related? do
               {:ok, new_expr} =
                 do_filter_with_related(
                   resource,
                   exists_expr,
                   path_filters,
                   prefix ++ at_path ++ exists_path
                 )

               {:halt, %{exists | expr: new_expr}}
             else
               primary_read_action = Ash.Resource.Info.primary_action!(unrelated_resource, :read)
               filter_key = {:unrelated_exists, unrelated_resource, primary_read_action.name}

               case Map.get(path_filters, filter_key) do
                 nil ->
                   {:ok, new_expr} =
                     do_filter_with_related(
                       unrelated_resource,
                       exists_expr,
                       path_filters,
                       []
                     )

                   {:halt, %{exists | expr: new_expr}}

                 %Ash.Filter{expression: false} ->
                   {:halt, false}

                 auth_filter ->
                   {:ok, new_expr} =
                     do_filter_with_related(
                       unrelated_resource,
                       exists_expr,
                       path_filters,
                       []
                     )

                   combined_expr =
                     Ash.Query.BooleanExpression.optimized_new(
                       :and,
                       new_expr,
                       auth_filter.expression
                     )

                   {:halt, %{exists | expr: combined_expr}}
               end
             end

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

  defp get_shared_multitenancy(%{context: %{multitenancy: multitenancy}}) do
    multitenancy
  end

  defp get_shared_multitenancy(_) do
    nil
  end
end
