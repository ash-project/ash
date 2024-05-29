defmodule Ash.Actions.Update.Bulk do
  @moduledoc false

  require Ash.Query

  @spec run(Ash.Domain.t(), Enumerable.t() | Ash.Query.t(), atom(), input :: map, Keyword.t()) ::
          Ash.BulkResult.t()

  def run(domain, resource, action, input, opts, not_atomic_reason \\ nil)

  def run(domain, resource, action, input, opts, not_atomic_reason) when is_atom(resource) do
    run(domain, Ash.Query.new(resource), action, input, opts, not_atomic_reason)
  end

  def run(domain, %Ash.Query{} = query, action, input, opts, not_atomic_reason) do
    opts = set_strategy(opts, query.resource)

    opts =
      if opts[:return_notifications?] do
        Keyword.put(opts, :notify?, true)
      else
        opts
      end

    query =
      if query.__validated_for_action__ do
        query
      else
        query =
          Ash.Query.for_read(
            query,
            get_read_action(query.resource, opts).name,
            %{},
            actor: opts[:actor],
            tenant: opts[:tenant]
          )

        {query, _opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, query, opts)

        query
      end

    query = %{query | domain: domain}

    fully_atomic_changeset =
      cond do
        !Enum.empty?(query.before_action) || !Enum.empty?(query.after_action) ->
          {:not_atomic,
           "cannot atomically update a query if it has `before_action` or `after_action` hooks"}

        not_atomic_reason ->
          {:not_atomic, not_atomic_reason}

        :atomic not in opts[:strategy] ->
          {:not_atomic, "Not in requested strategies"}

        changeset = opts[:atomic_changeset] ->
          changeset

        Ash.DataLayer.data_layer_can?(query.resource, :update_query) ->
          private_context = %{
            actor: opts[:actor],
            tenant: opts[:tenant],
            authorize?: opts[:authorize?]
          }

          opts =
            Keyword.update(
              opts,
              :context,
              %{private: private_context},
              &Map.put(&1, :private, private_context)
            )

          Ash.Changeset.fully_atomic_changeset(query.resource, action, input, opts)

        true ->
          {:not_atomic, "data layer does not support updating a query"}
      end

    case fully_atomic_changeset do
      {:not_atomic, reason} ->
        read_opts =
          Keyword.take(opts, Ash.stream_opt_keys())

        read_opts =
          if stream_batch_size = opts[:stream_batch_size] do
            Keyword.put(read_opts, :batch_size, stream_batch_size)
          else
            read_opts
          end

        case Ash.Actions.Read.Stream.stream_strategy(
               query,
               nil,
               opts[:allow_stream_with] || :keyset
             ) do
          {:error, %Ash.Error.Invalid.NonStreamableAction{} = exception} ->
            %Ash.BulkResult{
              status: :error,
              error_count: 1,
              errors: [
                Ash.Error.to_error_class(
                  Ash.Error.Invalid.NoMatchingBulkStrategy.exception(
                    resource: query.resource,
                    action: action,
                    requested_strategies: opts[:strategy],
                    not_atomic_reason: reason,
                    not_stream_reason: "could not stream the query",
                    footer: "Non stream reason:\n\n" <> Exception.message(exception)
                  )
                )
              ]
            }

          _ ->
            query =
              Ash.Query.do_filter(query, opts[:filter])

            run(
              domain,
              Ash.stream!(
                query,
                Keyword.merge(read_opts,
                  authorize?: opts[:authorize?] && opts[:authorize_query?],
                  domain: domain
                )
              ),
              action,
              input,
              Keyword.merge(opts,
                resource: query.resource,
                input_was_stream?: false
              ),
              reason
            )
        end

      %Ash.Changeset{valid?: false, errors: errors} ->
        %Ash.BulkResult{
          status: :error,
          error_count: 1,
          errors: [Ash.Error.to_error_class(errors)]
        }

      atomic_changeset ->
        atomic_changeset =
          Ash.Changeset.filter(atomic_changeset, opts[:filter])

        {atomic_changeset, opts} =
          Ash.Actions.Helpers.set_context_and_get_opts(domain, atomic_changeset, opts)

        atomic_changeset = %{atomic_changeset | domain: domain}

        notify? =
          if opts[:notify?] do
            if Process.get(:ash_started_transaction?) do
              false
            else
              Process.put(:ash_started_transaction?, true)
              true
            end
          else
            false
          end

        try do
          context =
            struct(
              Ash.Resource.Change.Context,
              %{
                bulk?: true,
                actor: opts[:actor],
                tenant: opts[:tenant],
                tracer: opts[:tracer],
                authorize?: opts[:authorize?]
              }
            )

          has_after_batch_hooks? =
            Enum.any?(
              atomic_changeset.action.changes ++
                Ash.Resource.Info.changes(atomic_changeset.resource, atomic_changeset.action_type),
              fn
                %{change: {module, change_opts}} ->
                  function_exported?(module, :after_batch, 3) &&
                    module.batch_callbacks?(query, change_opts, context)

                _ ->
                  false
              end
            )

          # There are performance implications here. We probably need to explicitly enable
          # having after action hooks. Or perhaps we need to stream the ids and then bulk update
          # them.
          opts =
            if has_after_batch_hooks? || opts[:notify?] do
              Keyword.put(opts, :return_records?, true)
            else
              opts
            end

          context_key =
            case atomic_changeset.action.type do
              :update ->
                :bulk_update

              :destroy ->
                :bulk_destroy
            end

          if has_after_batch_hooks? && Keyword.get(opts, :transaction, true) do
            Ash.DataLayer.transaction(
              List.wrap(atomic_changeset.resource) ++ action.touches_resources,
              fn ->
                do_atomic_update(query, atomic_changeset, has_after_batch_hooks?, input, opts)
              end,
              opts[:timeout],
              %{
                type: context_key,
                metadata: %{
                  resource: query.resource,
                  action: atomic_changeset.action.name,
                  actor: opts[:actor]
                },
                data_layer_context: opts[:data_layer_context] || %{}
              }
            )
          else
            {:ok, do_atomic_update(query, atomic_changeset, has_after_batch_hooks?, input, opts)}
          end
          |> case do
            {:ok, bulk_result} ->
              notifications =
                if notify? do
                  List.wrap(bulk_result.notifications) ++
                    List.wrap(Process.delete(:ash_notifications))
                else
                  List.wrap(bulk_result.notifications)
                end

              if opts[:return_notifications?] do
                %{bulk_result | notifications: notifications}
              else
                if opts[:return_notifications?] do
                  bulk_result
                else
                  if notify? do
                    remaining_notifications = Ash.Notifier.notify(notifications)

                    Ash.Actions.Helpers.warn_missed!(atomic_changeset.resource, action, %{
                      resource_notifications: remaining_notifications
                    })

                    %{bulk_result | notifications: notifications}
                  else
                    bulk_result
                  end
                end
              end

            {:error, error} ->
              %Ash.BulkResult{
                status: :error,
                errors: [Ash.Error.to_ash_error(error)],
                error_count: 1
              }
          end
        after
          if notify? do
            Process.put(:ash_started_transaction?, false)
          end
        end
    end
  end

  def run(domain, stream, action, input, opts, not_atomic_reason) do
    resource = opts[:resource]

    opts = set_strategy(opts, resource, Keyword.get(opts, :input_was_stream?, true))

    opts =
      if opts[:return_notifications?] do
        Keyword.put(opts, :notify?, true)
      else
        opts
      end

    not_atomic_reason =
      not_atomic_reason ||
        if :atomic_batches not in opts[:strategy],
          do: "Cannot perform atomic destroys on an enumerable of inputs"

    action =
      case action do
        nil ->
          Ash.Resource.Info.primary_action!(resource, :update)

        name when is_atom(name) ->
          action = Ash.Resource.Info.action(resource, action)

          if !action do
            raise Ash.Error.Invalid.NoSuchAction, resource: resource, action: name, type: :update
          end

          action

        action ->
          action
      end

    if opts[:transaction] == :all && opts[:return_stream?] do
      raise ArgumentError,
            "Cannot specify `transaction: :all` and `return_stream?: true` together"
    end

    if opts[:return_stream?] && opts[:sorted?] do
      raise ArgumentError, "Cannot specify `sorted?: true` and `return_stream?: true` together"
    end

    {context_key, metadata_key} =
      case action.type do
        :update ->
          {:bulk_update, :bulk_update_index}

        :destroy ->
          {:bulk_destroy, :bulk_destroy_index}
      end

    if opts[:transaction] == :all &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      notify? =
        if Process.get(:ash_started_transaction?) do
          false
        else
          Process.put(:ash_started_transaction?, true)
          true
        end

      Ash.DataLayer.transaction(
        List.wrap(resource) ++ action.touches_resources,
        fn ->
          do_run(
            domain,
            stream,
            action,
            input,
            opts,
            metadata_key,
            context_key,
            not_atomic_reason
          )
        end,
        opts[:timeout],
        %{
          type: context_key,
          metadata: %{
            resource: resource,
            action: action.name,
            actor: opts[:actor]
          },
          data_layer_context: opts[:data_layer_context] || %{}
        }
      )
      |> case do
        {:ok, bulk_result} ->
          bulk_result =
            if notify? do
              %{
                bulk_result
                | notifications:
                    bulk_result.notifications ++ Process.delete(:ash_notifications) || []
              }
            else
              bulk_result
            end

          handle_bulk_result(bulk_result, metadata_key, opts)

        {:error, error} ->
          {:error, error}
      end
    else
      domain
      |> do_run(stream, action, input, opts, metadata_key, context_key, not_atomic_reason)
      |> handle_bulk_result(metadata_key, opts)
    end
  end

  defp do_atomic_update(query, atomic_changeset, has_after_batch_hooks?, input, opts) do
    context =
      struct(
        Ash.Resource.Change.Context,
        %{
          bulk?: true,
          actor: opts[:actor],
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        }
      )

    atomic_changeset = Ash.Actions.Helpers.apply_opts_load(atomic_changeset, opts)

    atomic_changeset =
      if opts[:select] do
        Ash.Changeset.select(atomic_changeset, opts[:select])
      else
        atomic_changeset
      end

    atomic_changeset =
      if atomic_changeset.context[:data_layer][:use_atomic_update_data?] do
        atomic_changeset
      else
        %{
          atomic_changeset
          | data: %Ash.Changeset.OriginalDataNotAvailable{reason: :atomic_query_update}
        }
      end

    {all_changes, conditional_after_batch_hooks, calculations} =
      hooks_and_calcs_for_update_query(atomic_changeset, context, query, opts)

    update_query_opts =
      opts
      |> Keyword.take([:return_records?, :tenant])
      |> Map.new()
      |> Map.put(:calculations, calculations)

    with {:ok, query} <-
           authorize_bulk_query(query, atomic_changeset, opts),
         {:ok, atomic_changeset, query} <-
           authorize_atomic_changeset(query, atomic_changeset, opts),
         {query, atomic_changeset} <-
           add_changeset_filters(query, atomic_changeset),
         %Ash.Changeset{valid?: true} = atomic_changeset <-
           Ash.Changeset.handle_allow_nil_atomics(atomic_changeset, opts[:actor]),
         {:ok, data_layer_query} <-
           Ash.Query.data_layer_query(query) do
      case Ash.DataLayer.update_query(
             data_layer_query,
             atomic_changeset,
             update_query_opts
           ) do
        :ok ->
          %Ash.BulkResult{
            status: :success
          }

        {:ok, results} ->
          results =
            case results do
              [result] ->
                if atomic_changeset.context[:data_layer][:use_atomic_update_data?] do
                  Map.put(result, :__metadata__, atomic_changeset.data.__metadata__)
                else
                  [result]
                end

              other ->
                other
            end

          results = List.wrap(results)

          {results, notifications} =
            if has_after_batch_hooks? do
              run_atomic_after_batch_hooks(
                results,
                atomic_changeset,
                all_changes,
                conditional_after_batch_hooks,
                context
              )
            else
              {results, []}
            end

          {results, errors, error_count} =
            case load_data(
                   results,
                   atomic_changeset.domain,
                   atomic_changeset.resource,
                   atomic_changeset,
                   opts
                 ) do
              {:ok, results} ->
                {results, [], 0}

              {:error, error} ->
                {[], List.wrap(error), Enum.count(List.wrap(error))}
            end

          notifications =
            if opts[:notify?] do
              notifications ++
                Enum.map(results, fn result ->
                  notification(atomic_changeset, result, opts)
                end)
            else
              notifications
            end

          status =
            case {error_count, results} do
              {0, []} ->
                :success

              {0, _results} ->
                :success

              {_error_count, []} ->
                :error
            end

          %Ash.BulkResult{
            status: status,
            error_count: error_count,
            notifications: notifications,
            errors: errors,
            records:
              if opts[:return_records?] do
                results
              else
                []
              end
          }

        {:error, :no_rollback,
         %Ash.Error.Forbidden.Placeholder{
           authorizer: authorizer
         }} ->
          error =
            Ash.Authorizer.exception(
              authorizer,
              :forbidden,
              query.context[:private][:authorizer_state][authorizer]
            )

          %Ash.BulkResult{
            status: :error,
            error_count: 1,
            notifications: [],
            errors: [Ash.Error.to_error_class(error)]
          }

        {:error,
         %Ash.Error.Forbidden.Placeholder{
           authorizer: authorizer
         }} ->
          error =
            Ash.Authorizer.exception(
              authorizer,
              :forbidden,
              query.context[:private][:authorizer_state][authorizer]
            )

          if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
            Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
          else
            %Ash.BulkResult{
              status: :error,
              error_count: 1,
              notifications: [],
              errors: [Ash.Error.to_error_class(error)]
            }
          end

        {:error, :no_rollback, error} ->
          %Ash.BulkResult{
            status: :error,
            error_count: 1,
            notifications: [],
            errors: [Ash.Error.to_error_class(error)]
          }

        {:error, error} ->
          if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
            Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
          else
            %Ash.BulkResult{
              status: :error,
              error_count: 1,
              notifications: [],
              errors: [Ash.Error.to_error_class(error)]
            }
          end
      end
    else
      %Ash.Changeset{valid?: false, errors: error} ->
        if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
          Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
        else
          %Ash.BulkResult{
            status: :error,
            error_count: 1,
            notifications: [],
            errors: [Ash.Error.to_error_class(error)]
          }
        end

      {:error, %Ash.Error.Forbidden.InitialDataRequired{} = e} ->
        case Ash.Actions.Read.Stream.stream_strategy(
               query,
               nil,
               opts[:allow_stream_with] || :keyset
             ) do
          {:error, %Ash.Error.Invalid.NonStreamableAction{} = exception} ->
            %Ash.BulkResult{
              status: :error,
              error_count: 1,
              errors: [
                Ash.Error.to_error_class(
                  Ash.Error.Invalid.NoMatchingBulkStrategy.exception(
                    resource: atomic_changeset.resource,
                    action: atomic_changeset.action.name,
                    requested_strategies: opts[:strategy],
                    not_stream_reason: "could not stream the query, see message below for more",
                    not_atomic_batches_reason: "authorization requires initial data",
                    not_atomic_reason: "authorization requires initial data",
                    footer: "Non stream reason:\n\n" <> Exception.message(exception)
                  )
                )
              ]
            }

          _strategy ->
            input_stream =
              if atomic_changeset.context[:data_layer][:use_atomic_update_data?] do
                [atomic_changeset.data]
              else
                query
              end

            run(
              atomic_changeset.domain,
              input_stream,
              atomic_changeset.action,
              input,
              Keyword.merge(opts,
                authorize_query?: false,
                input_was_stream?: false
              ),
              e.source
            )
        end

      {:error, error} ->
        %Ash.BulkResult{
          status: :error,
          error_count: 1,
          notifications: [],
          errors: [Ash.Error.to_error_class(error)]
        }
    end
  end

  @doc false
  def run_atomic_after_batch_hooks(
        results,
        atomic_changeset,
        all_changes,
        conditional_after_batch_hooks,
        context
      ) do
    results =
      results
      |> Stream.with_index()
      |> Enum.map(fn {result, index} ->
        {atomic_changeset, Ash.Resource.set_metadata(result, %{atomic_index: index})}
      end)

    {records_with_changesets, notifications} =
      conditional_after_batch_hooks
      |> Enum.reduce(
        {results, []},
        fn {where, indices}, {records, additional_notifications} ->
          {to_apply, to_not_apply} =
            case where do
              true ->
                {records, []}

              false ->
                {[], records}

              where ->
                Enum.split_with(records, fn {_, record} ->
                  record.calculations[{:run_after_batch, where}]
                end)
            end

          if Enum.empty?(to_apply) do
            {records, additional_notifications}
          else
            {applied, notifications} =
              Enum.reduce(
                indices,
                {to_apply, additional_notifications},
                fn index, {records, additional_notifications} ->
                  change = Enum.at(all_changes, index)
                  {module, opts} = change.change

                  case module.after_batch(records, opts, context) do
                    :ok ->
                      {to_apply, additional_notifications}

                    instructions ->
                      Enum.reduce(
                        instructions,
                        {[], additional_notifications},
                        fn
                          {:ok, record}, {records, additional_notifications} ->
                            {[{atomic_changeset, record} | records], additional_notifications}

                          {:error, error}, _ ->
                            raise Ash.Error.to_ash_error(error)

                          %Ash.Notifier.Notification{} = notification,
                          {records, additional_notifications} ->
                            {records, [notification | additional_notifications]}
                        end
                      )
                  end
                end
              )

            {applied ++ to_not_apply, notifications ++ additional_notifications}
          end
        end
      )

    final_results =
      records_with_changesets
      # would be nice to have `Enum.map_sort_by`
      |> Stream.map(&elem(&1, 1))
      |> Enum.sort_by(& &1.__metadata__.atomic_index)

    {final_results, notifications}
  end

  @doc false
  def hooks_and_calcs_for_update_query(atomic_changeset, context, query, opts) do
    all_changes =
      atomic_changeset.action.changes
      |> Enum.concat(
        Ash.Resource.Info.changes(atomic_changeset.resource, atomic_changeset.action_type)
      )

    conditional_after_batch_hooks =
      all_changes
      |> Stream.with_index()
      |> Enum.filter(fn
        {%{change: {module, change_opts}}, _index} ->
          function_exported?(module, :after_batch, 3) &&
            module.batch_callbacks?(query, change_opts, context)

        _ ->
          false
      end)
      |> Enum.reduce(%{}, fn {%{where: where}, index}, acc ->
        {:atomic, condition} =
          Ash.Changeset.atomic_condition(where, atomic_changeset, context)

        Map.update(acc, condition, [index], &[index | &1])
      end)

    calculations =
      Enum.flat_map(conditional_after_batch_hooks, fn
        {static, _indices} when static in [true, false] ->
          []

        {where, _indices} ->
          {:ok, calculation} =
            Ash.Query.Calculation.new(
              {:run_after_batch, where},
              Ash.Resource.Calculation.Expression,
              [expression: where],
              :boolean,
              [],
              arguments: atomic_changeset.arguments,
              source_context: atomic_changeset.context
            )

          calculation =
            Ash.Actions.Read.add_calc_context(
              calculation,
              opts[:actor],
              opts[:authorize?],
              opts[:tenant],
              opts[:tracer],
              atomic_changeset.domain
            )

          [{calculation, where}]
      end)

    {all_changes, conditional_after_batch_hooks, calculations}
  end

  defp set_strategy(opts, resource, inputs_is_enumerable? \\ false) do
    opts =
      if Ash.DataLayer.data_layer_can?(resource, :update_query) do
        opts
      else
        Keyword.put(opts, :strategy, [:stream])
      end

    if inputs_is_enumerable? && :atomic in List.wrap(opts[:strategy]) do
      Keyword.put(opts, :strategy, Enum.uniq([:atomic_batches | List.wrap(opts[:strategy])]))
    else
      opts
    end
  end

  defp do_run(domain, stream, action, input, opts, metadata_key, context_key, not_atomic_reason) do
    resource = opts[:resource]
    opts = Ash.Actions.Helpers.set_opts(opts, domain)
    read_action = get_read_action(resource, opts)

    {context_cs, opts} =
      Ash.Actions.Helpers.set_context_and_get_opts(domain, Ash.Changeset.new(resource), opts)

    fully_atomic_changeset =
      cond do
        :atomic_batches not in opts[:strategy] ->
          {:not_atomic, "Not in requested strategies"}

        Enum.empty?(Ash.Resource.Info.primary_key(resource)) ->
          {:not_atomic, "cannot atomically update a stream without a primary key"}

        !read_action ->
          {:not_atomic, "cannot atomically update a stream without a primary read action"}

        Ash.DataLayer.data_layer_can?(resource, :update_query) ->
          opts =
            Keyword.update(
              opts,
              :context,
              %{private: context_cs.context.private},
              &Map.put(&1, :private, context_cs.context.private)
            )

          Ash.Changeset.fully_atomic_changeset(resource, action, input, opts)

        true ->
          {:not_atomic, "data layer does not support updating a query"}
      end

    case fully_atomic_changeset do
      %Ash.Changeset{} = atomic_changeset ->
        query =
          resource
          |> Ash.Query.new()
          |> Map.put(:action, read_action)

        case Ash.Actions.Read.Stream.stream_strategy(
               query,
               nil,
               opts[:allow_stream_with] || :keyset
             ) do
          {:error, exception} ->
            if :stream in opts[:strategy] do
              do_stream_batches(domain, stream, action, input, opts, metadata_key, context_key)
            else
              %Ash.BulkResult{
                status: :error,
                error_count: 1,
                errors: [
                  Ash.Error.to_error_class(
                    Ash.Error.Invalid.NoMatchingBulkStrategy.exception(
                      resource: resource,
                      action: action.name,
                      requested_strategies: opts[:strategy],
                      not_atomic_batches_reason: "could not stream the query",
                      not_atomic_reason: not_atomic_reason,
                      not_stream_reason: "could not stream the query",
                      footer: "Non stream reason:\n\n" <> Exception.message(exception)
                    )
                  )
                ]
              }
            end

          _ ->
            do_atomic_batches(
              atomic_changeset,
              domain,
              stream,
              action,
              input,
              opts
            )
        end

      {:not_atomic, not_atomic_batches_reason} ->
        if :stream in opts[:strategy] do
          do_stream_batches(domain, stream, action, input, opts, metadata_key, context_key)
        else
          %Ash.BulkResult{
            status: :error,
            error_count: 1,
            errors: [
              Ash.Error.to_error_class(
                Ash.Error.Invalid.NoMatchingBulkStrategy.exception(
                  resource: resource,
                  action: action.name,
                  requested_strategies: opts[:strategy],
                  not_atomic_batches_reason: not_atomic_batches_reason,
                  not_atomic_reason: not_atomic_reason
                )
              )
            ]
          }
        end
    end
  end

  defp do_atomic_batches(
         atomic_changeset,
         domain,
         stream,
         action,
         input,
         opts
       ) do
    batch_size = opts[:batch_size] || 100
    resource = opts[:resource]
    ref = make_ref()
    pkey = Ash.Resource.Info.primary_key(resource)

    stream
    |> Stream.chunk_every(batch_size)
    |> map_batches(
      resource,
      opts,
      ref,
      fn batch ->
        pkeys = [or: Enum.map(batch, &Map.take(&1, pkey))]

        resource
        |> Ash.Query.for_read(get_read_action(resource, opts).name, %{},
          actor: opts[:actor],
          authorize?: false,
          context: atomic_changeset.context,
          tenant: atomic_changeset.tenant,
          tracer: opts[:tracer]
        )
        |> Ash.Query.set_context(%{private: %{internal?: true}})
        |> Ash.Query.filter(^pkeys)
        |> Ash.Query.filter(^atomic_changeset.filter)
        |> Ash.Query.select([])
        |> then(fn query ->
          run(domain, query, action.name, input,
            actor: opts[:actor],
            authorize_query?: false,
            authorize?: opts[:authorize?],
            tenant: atomic_changeset.tenant,
            context: opts[:context] || %{},
            tracer: opts[:tracer],
            atomic_changeset: atomic_changeset,
            load: opts[:load],
            resource: opts[:resource],
            input_was_stream?: false,
            return_errors?: opts[:return_errors?],
            filter: opts[:filter],
            return_notifications?: opts[:return_notifications?],
            notify?: opts[:notify?],
            return_records?: opts[:return_records?],
            strategy: [:atomic]
          )
          |> case do
            %Ash.BulkResult{error_count: 0, records: records, notifications: notifications} ->
              store_notification(ref, notifications, opts)
              List.wrap(records)

            %Ash.BulkResult{
              errors: errors,
              notifications: notifications,
              error_count: error_count
            } ->
              store_notification(ref, notifications, opts)
              store_error(ref, errors, opts, error_count)
              []
          end
        end)
      end
    )
    |> run_batches(ref, opts)
  end

  defp do_stream_batches(domain, stream, action, input, opts, metadata_key, context_key) do
    resource = opts[:resource]

    manual_action_can_bulk? =
      case action.manual do
        {mod, _opts} ->
          function_exported?(mod, :bulk_update, 3)

        _ ->
          false
      end

    batch_size =
      if action.manual == nil || manual_action_can_bulk? do
        opts[:batch_size] || 100
      else
        1
      end

    ref = make_ref()

    base_changeset = base_changeset(resource, domain, opts, action, input)

    all_changes =
      pre_template_all_changes(action, resource, action.type, base_changeset, opts[:actor])

    argument_names = Enum.map(action.arguments, & &1.name)

    stream
    |> Stream.with_index()
    |> Stream.chunk_every(batch_size)
    |> map_batches(
      resource,
      opts,
      ref,
      fn batch ->
        try do
          batch
          |> Enum.map(
            &setup_changeset(
              &1,
              action,
              opts,
              input,
              argument_names,
              domain,
              context_key
            )
          )
          |> handle_batch(
            domain,
            resource,
            action,
            all_changes,
            opts,
            ref,
            context_key,
            metadata_key,
            base_changeset
          )
        after
          if opts[:notify?] && !opts[:return_notifications?] do
            Ash.Notifier.notify(Process.delete({:bulk_update_notifications, ref}))
          end
        end
      end
    )
    |> run_batches(ref, opts)
  end

  defp run_batches(changeset_stream, ref, opts) do
    if opts[:return_stream?] do
      Stream.concat(changeset_stream)
    else
      try do
        records =
          if opts[:return_records?] do
            Enum.to_list(Stream.concat(changeset_stream))
          else
            Stream.run(changeset_stream)
            []
          end

        notifications =
          if opts[:notify?] && opts[:return_notifications?] do
            Process.delete({:bulk_update_notifications, ref})
          else
            if opts[:notify?] do
              Ash.Notifier.notify(Process.delete({:bulk_update_notifications, ref}))
            end

            []
          end

        {errors, error_count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

        bulk_result = %Ash.BulkResult{
          records: records,
          errors: errors,
          notifications: notifications,
          error_count: error_count
        }

        case bulk_result do
          %{records: _, error_count: 0} -> %{bulk_result | status: :success}
          %{records: [], error_count: _} -> %{bulk_result | status: :error}
          _ -> %{bulk_result | status: :partial_success}
        end
      catch
        {:error, error, batch_number} ->
          status =
            if batch_number > 1 do
              :partial_success
            else
              :error
            end

          result = %Ash.BulkResult{
            status: status,
            notifications: Process.delete({:bulk_update_notifications, ref})
          }

          {error_count, errors} = errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_update_errors, ref})
        Process.delete({:bulk_update_notifications, ref})
      end
    end
  end

  defp authorize_bulk_query(query, atomic_changeset, opts) do
    if opts[:authorize?] && opts[:authorize_query?] do
      case Ash.can(query, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             pre_flight?: false,
             atomic_changeset: atomic_changeset,
             filter_with: opts[:authorize_query_with] || :filter,
             run_queries?: false,
             alter_source?: true,
             no_check?: true
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

  defp authorize_atomic_changeset(query, changeset, opts) do
    if opts[:authorize?] do
      case Ash.can(
             changeset,
             opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             atomic_changeset: changeset,
             pre_flight?: false,
             no_check?: true,
             on_must_pass_strict_check:
               {:error,
                %Ash.Error.Forbidden.InitialDataRequired{source: "must pass strict check"}},
             filter_with: opts[:authorize_changeset_with] || :filter,
             alter_source?: true,
             run_queries?: false,
             base_query: query
           ) do
        {:ok, true} ->
          {:ok, changeset, query}

        {:ok, true, %Ash.Query{} = query} ->
          {:ok, changeset, query}

        {:ok, true, %Ash.Changeset{} = changeset} ->
          {:ok, changeset, query}

        {:ok, true, %Ash.Changeset{} = changeset, %Ash.Query{} = query} ->
          {:ok, changeset, query}

        {:ok, false, error} ->
          {:error, error}

        {:error, error} ->
          {:error, error}
      end
    else
      {:ok, changeset, query}
    end
  end

  defp base_changeset(resource, domain, opts, action, input) do
    arguments =
      Enum.reduce(input, %{}, fn {key, value}, acc ->
        argument =
          if is_binary(key) do
            Enum.find(action.arguments, fn arg ->
              to_string(arg.name) == key
            end)
          else
            Enum.find(action.arguments, fn arg ->
              arg.name == key
            end)
          end

        if argument do
          Map.put(acc, argument.name, value)
        else
          acc
        end
      end)

    resource
    |> Ash.Changeset.new()
    |> Map.put(:domain, domain)
    |> Ash.Changeset.filter(opts[:filter])
    |> Ash.Actions.Helpers.add_context(opts)
    |> Ash.Changeset.set_context(opts[:context] || %{})
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
    |> Ash.Changeset.set_arguments(arguments)
    |> then(fn changeset ->
      changeset =
        if select = opts[:select] do
          Ash.Changeset.select(changeset, select)
        else
          changeset
        end

      if load = opts[:load] do
        Ash.Changeset.load(changeset, load)
      else
        changeset
      end
    end)
  end

  defp pre_template_all_changes(action, resource, _type, base, actor) do
    action.changes
    |> Enum.concat(Ash.Resource.Info.validations(resource, action.type))
    |> Enum.concat(Ash.Resource.Info.changes(resource, action.type))
    |> Enum.map(fn
      %{change: {module, opts}} = change ->
        %{change | change: {module, pre_template(opts, base, actor)}}

      %{validation: {module, opts}} = validation ->
        %{validation | validation: {module, pre_template(opts, base, actor)}}
    end)
    |> Enum.map(fn
      %{where: where} = change ->
        new_where =
          if where do
            where
            |> List.wrap()
            |> Enum.map(fn {module, opts} -> {module, pre_template(opts, base, actor)} end)
          end

        %{change | where: new_where}

      other ->
        other
    end)
    |> Enum.with_index()
  end

  defp add_changeset_filters(query, changeset) do
    {Ash.Query.do_filter(query, changeset.filter), %{changeset | filter: nil}}
  end

  defp pre_template(opts, changeset, actor) do
    if Ash.Expr.template_references_context?(opts) do
      opts
    else
      {:templated,
       Ash.Expr.fill_template(
         opts,
         actor,
         changeset.arguments,
         changeset.context
       )}
    end
  end

  defp error_stream(ref) do
    the_errors = Process.delete({:bulk_update_errors, ref})

    Stream.resource(
      fn -> the_errors end,
      fn
        {errors, _count} ->
          {Stream.map(errors || [], &{:error, &1}), []}

        _ ->
          {:halt, []}
      end,
      fn _ -> :ok end
    )
  end

  defp notification_stream(ref) do
    the_notifications = Process.delete({:bulk_update_notifications, ref})

    Stream.resource(
      fn -> the_notifications end,
      fn
        [] ->
          {:halt, []}

        notifications ->
          {Stream.map(notifications || [], &{:notification, &1}), []}
      end,
      fn _ -> :ok end
    )
  end

  defp handle_batch(
         batch,
         domain,
         resource,
         action,
         all_changes,
         opts,
         ref,
         context_key,
         metadata_key,
         base_changeset
       ) do
    %{
      must_return_records?: must_return_records_for_changes?,
      batch: batch,
      changes: changes
    } =
      run_action_changes(
        batch,
        all_changes,
        action,
        opts[:actor],
        opts[:authorize?],
        opts[:tracer],
        opts[:tenant],
        context_key
      )

    {batch, must_be_simple} =
      Enum.reduce(batch, {[], []}, fn changeset, {batch, must_be_simple} ->
        if changeset.after_transaction in [[], nil] do
          changeset = Ash.Changeset.run_before_transaction_hooks(changeset)
          {[changeset | batch], must_be_simple}
        else
          {batch, [%{changeset | __validated_for_action__: action.name} | must_be_simple]}
        end
      end)

    must_be_simple_results =
      Enum.flat_map(must_be_simple, fn changeset ->
        case Ash.Actions.Update.run(
               domain,
               changeset,
               action,
               Keyword.put(opts, :atomic_upgrade?, false)
             ) do
          {:ok, result} ->
            [
              Ash.Resource.set_metadata(result, %{
                metadata_key => changeset.context |> Map.get(context_key) |> Map.get(:index)
              })
            ]

          {:error, error} ->
            store_error(ref, error, opts)
            []
        end
      end)

    if opts[:transaction] == :batch &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      context = batch |> Enum.at(0) |> Kernel.||(%{}) |> Map.get(:context)

      notify? =
        if opts[:notify?] do
          if Process.get(:ash_started_transaction?) do
            false
          else
            Process.put(:ash_started_transaction?, true)
            true
          end
        else
          false
        end

      try do
        Ash.DataLayer.transaction(
          List.wrap(resource) ++ action.touches_resources,
          fn ->
            tmp_ref = make_ref()

            result =
              do_handle_batch(
                batch,
                domain,
                resource,
                action,
                opts,
                all_changes,
                tmp_ref,
                metadata_key,
                context_key,
                base_changeset,
                must_return_records_for_changes?,
                changes,
                must_be_simple_results
              )

            {new_errors, new_error_count} =
              Process.delete({:bulk_update_errors, tmp_ref}) || {[], 0}

            store_error(ref, new_errors, opts, new_error_count)

            result
          end,
          opts[:timeout],
          %{
            type: context_key,
            metadata: %{
              resource: resource,
              action: action.name,
              actor: opts[:actor]
            },
            data_layer_context: opts[:data_layer_context] || context
          }
        )
        |> case do
          {:ok, result} ->
            result

          {:error, error} ->
            store_error(ref, error, opts)

            []
        end
      after
        if notify? do
          notifications = Process.get(:ash_notifications, [])
          remaining_notifications = Ash.Notifier.notify(notifications)
          Process.delete(:ash_notifications) || []

          Ash.Actions.Helpers.warn_missed!(resource, action, %{
            resource_notifications: remaining_notifications
          })
        end
      end
    else
      do_handle_batch(
        batch,
        domain,
        resource,
        action,
        opts,
        all_changes,
        ref,
        metadata_key,
        context_key,
        base_changeset,
        must_return_records_for_changes?,
        changes,
        must_be_simple_results
      )
    end
  end

  defp do_handle_batch(
         batch,
         domain,
         resource,
         action,
         opts,
         all_changes,
         ref,
         metadata_key,
         context_key,
         base_changeset,
         must_return_records_for_changes?,
         changes,
         must_be_simple_results
       ) do
    must_return_records? =
      opts[:notify?] ||
        Enum.any?(batch, fn item ->
          item.after_action != []
        end)

    batch =
      batch
      |> authorize(opts)
      |> run_bulk_before_batches(
        changes,
        all_changes,
        opts,
        ref,
        context_key
      )

    run_batch(
      resource,
      batch,
      action,
      opts,
      must_return_records?,
      must_return_records_for_changes?,
      domain,
      ref,
      metadata_key,
      context_key
    )
    |> run_after_action_hooks(opts, domain, ref, metadata_key)
    |> process_results(
      changes,
      all_changes,
      opts,
      ref,
      batch,
      metadata_key,
      resource,
      domain,
      base_changeset
    )
    |> then(fn stream ->
      if opts[:return_stream?] do
        stream
        |> Stream.concat(must_be_simple_results)
        |> Stream.map(&{:ok, &1})
        |> Stream.concat(error_stream(ref))
        |> Stream.concat(notification_stream(ref))
      else
        Enum.concat(stream, must_be_simple_results)
      end
    end)
  end

  defp setup_changeset(
         {record, index},
         action,
         opts,
         input,
         argument_names,
         domain,
         context_key
       ) do
    record
    |> Ash.Changeset.new()
    |> Map.put(:domain, domain)
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
    |> Ash.Changeset.put_context(context_key, %{index: index})
    |> Ash.Changeset.atomic_update(opts[:atomic_update] || [])
    |> Ash.Changeset.hydrate_atomic_refs(opts[:actor], opts)
    |> handle_params(
      Keyword.get(opts, :assume_casted?, false),
      action,
      opts,
      input,
      argument_names
    )
  end

  defp handle_params(changeset, false, action, opts, input, _argument_names) do
    Ash.Changeset.handle_params(changeset, action, input, opts)
  end

  defp handle_params(changeset, true, action, opts, input, argument_names) do
    {args, attrs} =
      Map.split(input, argument_names)

    %{changeset | arguments: args, attributes: attrs}
    |> Ash.Changeset.handle_params(action, input, Keyword.put(opts, :cast_params?, false))
  end

  defp map_batches(stream, resource, opts, ref, callback) do
    max_concurrency = opts[:max_concurrency]

    max_concurrency =
      if max_concurrency && max_concurrency > 1 && Ash.DataLayer.can?(:async_engine, resource) do
        max_concurrency
      else
        0
      end

    if max_concurrency && max_concurrency > 1 do
      Task.async_stream(
        stream,
        fn batch ->
          Process.put(:ash_started_transaction?, true)
          batch_result = callback.(batch)
          {errors, _} = Process.get({:bulk_update_errors, ref}) || {[], 0}

          notifications =
            if opts[:notify?] do
              process_notifications = Process.get(:ash_notifications, [])
              bulk_notifications = Process.get({:bulk_update_notifications, ref}) || []

              if opts[:return_notifications?] do
                process_notifications ++ bulk_notifications
              else
                if opts[:transaction] && opts[:transaction] != :all do
                  Ash.Notifier.notify(bulk_notifications)
                  Ash.Notifier.notify(process_notifications)
                end

                []
              end
            end

          {batch_result, notifications, errors}
        end,
        timeout: :infinity,
        max_concurrency: max_concurrency
      )
      |> Stream.map(fn
        {:ok, {:throw, value}} ->
          throw(value)

        {:ok,
         {%Ash.BulkResult{
            records: records,
            notifications: notifications,
            errors: errors,
            error_count: error_count
          }, _, _}} ->
          store_notification(ref, notifications, opts)
          store_error(ref, errors, opts, error_count)
          records

        {:ok, {result, notifications, errors}} ->
          store_notification(ref, notifications, opts)
          store_error(ref, errors, opts)

          result

        {:exit, error} ->
          store_error(ref, error, opts)
          []
      end)
    else
      Stream.map(stream, callback)
    end
  end

  defp index_changesets(batch, context_key) do
    Enum.reduce(batch, %{}, fn changeset, changesets_by_index ->
      Map.put(
        changesets_by_index,
        changeset.context[context_key].index,
        changeset
      )
    end)
  end

  defp errors(result, invalid, opts) when is_list(invalid) do
    Enum.reduce(invalid, {result.error_count, result.errors}, fn invalid, {error_count, errors} ->
      errors(%{result | error_count: error_count, errors: errors}, invalid, opts)
    end)
  end

  defp errors(result, nil, _opts) do
    {result.error_count + 1, []}
  end

  defp errors(result, {:error, error}, opts) do
    if opts[:return_errors?] do
      {result.error_count + 1, [error | result.errors]}
    else
      {result.error_count + 1, []}
    end
  end

  defp errors(result, invalid, opts) do
    if Enumerable.impl_for(invalid) do
      invalid = Enum.to_list(invalid)
      errors(result, invalid, opts)
    else
      errors(result, {:error, invalid}, opts)
    end
  end

  defp run_bulk_before_batches(
         batch,
         changes,
         all_changes,
         opts,
         ref,
         context_key
       ) do
    all_changes
    |> Enum.filter(fn
      {%{change: {module, _opts}}, _} ->
        function_exported?(module, :before_batch, 3)

      _ ->
        false
    end)
    |> Enum.reduce(batch, fn {%{change: {module, change_opts}}, index}, batch ->
      # change may return a stream but before_batch/3 expects a list
      batch = Enum.to_list(batch)

      if changes[index] == :all do
        module.before_batch(
          batch,
          change_opts,
          struct(Ash.Resource.Change.Context, %{
            bulk?: true,
            actor: opts[:actor],
            tenant: opts[:tenant],
            tracer: opts[:tracer],
            authorize?: opts[:authorize?]
          })
        )
      else
        {matches, non_matches} =
          batch
          |> Enum.split_with(fn
            %{valid?: false} ->
              false

            changeset ->
              changeset.context[context_key].index in List.wrap(changes[index])
          end)

        before_batch_results =
          module.before_batch(
            matches,
            change_opts,
            struct(Ash.Resource.Change.Context, %{
              bulk?: true,
              actor: opts[:actor],
              tenant: opts[:tenant],
              tracer: opts[:tracer],
              authorize?: opts[:authorize?]
            })
          )

        Enum.concat([before_batch_results, non_matches])
      end
    end)
    |> Enum.reject(fn
      %Ash.Notifier.Notification{} = notification ->
        store_notification(ref, notification, opts)
        true

      _changeset ->
        false
    end)
  end

  defp store_error(ref, errors, opts, count \\ nil)

  defp store_error(_ref, empty, _opts, 0) when empty in [[], nil], do: :ok

  defp store_error(ref, empty, _opts, error_count) when empty in [[], nil] do
    {errors, count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

    Process.put(
      {:bulk_update_errors, ref},
      {errors, count + (error_count || 1)}
    )
  end

  defp store_error(ref, error, opts, count) do
    add = count || Enum.count(List.wrap(error))

    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error), 0, []})
    else
      if opts[:return_errors?] do
        {errors, count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

        new_errors =
          error
          |> List.wrap()
          |> Enum.map(&Ash.Error.to_ash_error/1)

        Process.put(
          {:bulk_update_errors, ref},
          {new_errors ++ errors, count + add}
        )
      else
        {errors, count} = Process.get({:bulk_update_errors, ref}) || {[], 0}
        Process.put({:bulk_update_errors, ref}, {errors, count + add})
      end
    end
  end

  defp store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_notification(ref, notification, opts) do
    if opts[:notify?] || opts[:return_notifications?] do
      notifications = Process.get({:bulk_update_notifications, ref}) || []

      new_notifications =
        if is_list(notification) do
          notification ++ notifications
        else
          [notification | notifications]
        end

      Process.put({:bulk_update_notifications, ref}, new_notifications)
    end
  end

  defp authorize(batch, opts) do
    if opts[:authorize?] do
      batch
      |> Enum.map(fn changeset ->
        if changeset.valid? do
          case Ash.can(changeset, opts[:actor],
                 return_forbidden_error?: true,
                 maybe_is: false,
                 pre_flight?: false
               ) do
            {:ok, true} ->
              changeset

            {:ok, false, error} ->
              Ash.Changeset.add_error(changeset, error)

            {:error, error} ->
              Ash.Changeset.add_error(changeset, error)
          end
        else
          changeset
        end
      end)
    else
      batch
    end
  end

  defp handle_bulk_result(%Ash.BulkResult{} = bulk_result, metadata_key, opts) do
    bulk_result
    |> sort(metadata_key, opts)
    |> ensure_records_return_type(opts)
    |> ensure_errors_return_type(opts)
  end

  # for when we return a stream
  defp handle_bulk_result(stream, _, _), do: stream

  defp ensure_records_return_type(result, opts) do
    if opts[:return_records?] do
      %{result | records: result.records || []}
    else
      %{result | records: nil}
    end
  end

  defp ensure_errors_return_type(result, opts) do
    if opts[:return_errors?] do
      %{result | errors: result.errors || []}
    else
      %{result | errors: nil}
    end
  end

  defp sort(%{records: records} = result, metadata_key, opts) when is_list(records) do
    if opts[:sorted?] do
      %{result | records: Enum.sort_by(records, & &1.__metadata__[metadata_key])}
    else
      result
    end
  end

  defp sort(result, _action, _), do: result

  defp run_batch(
         resource,
         batch,
         action,
         opts,
         must_return_records?,
         must_return_records_for_changes?,
         domain,
         ref,
         metadata_key,
         context_key
       ) do
    context_struct =
      case context_key do
        :bulk_update ->
          Ash.Resource.ManualUpdate.Context

        :bulk_destroy ->
          Ash.Resource.ManualDestroy.Context
      end

    batch =
      Enum.map(batch, fn changeset ->
        changeset = Ash.Changeset.hydrate_atomic_refs(changeset, opts[:actor], opts)

        if changeset.valid? do
          {changeset, %{notifications: new_notifications}} =
            Ash.Changeset.run_before_actions(changeset)

          changeset =
            changeset
            |> Ash.Changeset.handle_allow_nil_atomics(opts[:actor])
            |> Ash.Changeset.require_values(
              :update,
              true
            )

          changed? =
            Ash.Changeset.changing_attributes?(changeset) or
              not Enum.empty?(changeset.atomics)

          changeset =
            Ash.Changeset.put_context(changeset, :changed?, changed?)

          new_notifications = store_notification(ref, new_notifications, opts)

          {changeset, manage_notifications} =
            if changeset.valid? do
              case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                     changeset,
                     opts[:actor],
                     authorize?: opts[:authorize?],
                     actor: opts[:actor],
                     tenant: opts[:tenant]
                   ) do
                {:error, error} ->
                  {Ash.Changeset.add_error(changeset, error), new_notifications}

                {changeset, manage_instructions} ->
                  {changeset, manage_instructions.notifications}
              end
            else
              {changeset, []}
            end

          store_notification(ref, manage_notifications, opts)

          changeset
        else
          changeset
        end
      end)

    changesets_by_index = index_changesets(batch, context_key)

    batch =
      batch
      |> Enum.reject(fn
        %{valid?: false} = changeset ->
          store_error(ref, changeset, opts)
          true

        _changeset ->
          false
      end)
      |> case do
        [] ->
          []

        batch ->
          batch
          |> Enum.group_by(&{&1.atomics, &1.filter})
          |> Enum.flat_map(fn {_atomics, batch} ->
            result =
              case action.manual do
                {mod, opts} ->
                  if function_exported?(mod, context_key, 3) do
                    apply(mod, context_key, [
                      batch,
                      opts,
                      struct(context_struct,
                        actor: opts[:actor],
                        select: opts[:select],
                        batch_size: opts[:batch_size],
                        authorize?: opts[:authorize?],
                        tracer: opts[:tracer],
                        domain: domain,
                        return_records?:
                          opts[:return_records?] || must_return_records? ||
                            must_return_records_for_changes?,
                        tenant: Ash.ToTenant.to_tenant(opts[:tenant], resource)
                      )
                    ])
                    |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                    |> case do
                      {:ok, result} ->
                        [result]

                      :ok ->
                        if opts[:return_records?] do
                          raise "`#{inspect(mod)}.#{context_key}/3` returned :ok without a result when `return_records?` is true"
                        else
                          []
                        end

                      {:error, error} ->
                        store_error(ref, error, opts)
                        []

                      {:notifications, notifications} ->
                        store_notification(ref, notifications, opts)
                        []
                    end
                  else
                    [changeset] = batch

                    result =
                      apply(mod, action.type, [
                        changeset,
                        opts,
                        struct(context_struct, %{
                          select: opts[:select],
                          actor: opts[:actor],
                          tenant: opts[:tenant],
                          authorize?: opts[:authorize?],
                          tracer: opts[:tracer],
                          domain: domain
                        })
                      ])

                    case result do
                      {:ok, result} ->
                        {:ok,
                         [
                           Ash.Resource.put_metadata(
                             result,
                             metadata_key,
                             changeset.context[context_key].index
                           )
                         ]}

                      {:error, error} ->
                        {:error, error}
                    end
                  end

                _ ->
                  Enum.reduce_while(
                    batch,
                    {:ok, []},
                    fn changeset, {:ok, results} ->
                      resource
                      |> Ash.DataLayer.update(changeset)
                      |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                      |> case do
                        {:ok, result} ->
                          result =
                            Ash.Resource.put_metadata(
                              result,
                              metadata_key,
                              changeset.context[context_key].index
                            )

                          {:cont, {:ok, [result | results]}}

                        {:error, %Ash.Error.Changes.StaleRecord{}} ->
                          {:cont, {:ok, results}}

                        {:error, error} ->
                          {:halt, {:error, error}}
                      end
                    end
                  )
                  |> case do
                    {:ok, results} ->
                      {:ok, Enum.reverse(results)}

                    {:error, error} ->
                      {:error, error}
                  end
              end

            case result do
              {:ok, result} ->
                result

              {:error, error} ->
                store_error(ref, error, opts)
                []
            end
          end)
      end

    {batch, changesets_by_index}
  end

  defp manage_relationships(updated, domain, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(domain, updated, changeset, engine_opts),
         {:ok, with_relationships, new_notifications} <-
           Ash.Actions.ManagedRelationships.manage_relationships(
             loaded,
             changeset,
             engine_opts[:actor],
             engine_opts
           ) do
      {:ok, with_relationships, %{notifications: new_notifications, new_changeset: changeset}}
    end
  end

  defp run_after_action_hooks(
         {batch_results, changesets_by_index},
         opts,
         domain,
         ref,
         metadata_key
       ) do
    results =
      Enum.flat_map(batch_results, fn result ->
        changeset = changesets_by_index[result.__metadata__[metadata_key]]

        case manage_relationships(result, domain, changeset,
               actor: opts[:actor],
               authorize?: opts[:authorize?]
             ) do
          {:ok, result, %{notifications: new_notifications, new_changeset: changeset}} ->
            store_notification(ref, new_notifications, opts)

            case Ash.Changeset.run_after_actions(result, changeset, []) do
              {:error, error} ->
                if opts[:transaction] && opts[:rollback_on_error?] do
                  if Ash.DataLayer.in_transaction?(changeset.resource) do
                    Ash.DataLayer.rollback(
                      changeset.resource,
                      error
                    )
                  end
                end

                store_error(ref, error, opts)
                []

              {:ok, result, _changeset, %{notifications: more_new_notifications}} ->
                store_notification(ref, more_new_notifications, opts)
                [result]
            end

          {:error, error} ->
            store_error(ref, error, opts)
            []
        end
      end)

    {results, changesets_by_index}
  end

  defp process_results(
         {batch, changesets_by_index},
         changes,
         all_changes,
         opts,
         ref,
         changesets,
         metadata_key,
         resource,
         domain,
         base_changeset
       ) do
    run_bulk_after_changes(
      changes,
      all_changes,
      batch,
      changesets_by_index,
      changesets,
      opts,
      ref,
      base_changeset.resource,
      metadata_key
    )
    |> Enum.flat_map(fn result ->
      changeset = changesets_by_index[result.__metadata__[metadata_key]]

      if opts[:notify?] || opts[:return_notifications?] do
        store_notification(ref, notification(changeset, result, opts), opts)
      end

      try do
        case Ash.Changeset.run_after_transactions(
               {:ok, result},
               changeset
             ) do
          {:ok, result} ->
            if opts[:return_records?] do
              [result]
            else
              []
            end

          {:error, error} ->
            store_error(ref, error, opts)
            []
        end
      rescue
        e ->
          store_error(ref, e, opts)
          []
      end
    end)
    |> load_data(domain, resource, base_changeset, opts)
    |> case do
      {:ok, records} ->
        Enum.reject(records, & &1.__metadata__[:private][:missing_from_data_layer])

      {:error, error} ->
        store_error(ref, error, opts)
        []
    end
  end

  defp load_data(records, domain, resource, changeset, opts) do
    select =
      if changeset.select do
        List.wrap(changeset.select)
      else
        resource |> Ash.Resource.Info.public_attributes() |> Enum.map(& &1.name)
      end

    case Ash.load(records, select,
           reuse_values?: true,
           domain: domain,
           actor: opts[:actor],
           authorize?: opts[:authorize?],
           tracer: opts[:tracer]
         ) do
      {:ok, records} ->
        Ash.load(
          records,
          List.wrap(changeset.load),
          reuse_values?: true,
          domain: domain,
          actor: opts[:actor],
          authorize?: opts[:authorize?],
          tracer: opts[:tracer]
        )
        |> Ash.Actions.Helpers.select(changeset)

      other ->
        other
    end
  end

  defp run_bulk_after_changes(
         changes,
         all_changes,
         results,
         changesets_by_index,
         changesets,
         opts,
         ref,
         resource,
         metadata_key
       ) do
    context =
      struct(
        Ash.Resource.Change.Context,
        %{
          bulk?: true,
          actor: opts[:actor],
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        }
      )

    all_changes
    |> Enum.filter(fn
      {%{change: {module, change_opts}}, _} ->
        function_exported?(module, :after_batch, 3) &&
          function_exported?(module, :batch_change, 3) &&
          module.batch_callbacks?(changesets, change_opts, context)

      _ ->
        false
    end)
    |> Enum.reduce(results, fn {%{change: {module, change_opts}}, index}, results ->
      records = results

      if changes[index] == :all do
        results =
          Enum.map(results, fn result ->
            {changesets_by_index[result.__metadata__[metadata_key]], result}
          end)

        module.after_batch(
          results,
          change_opts,
          context
        )
        |> handle_after_batch_results(records, ref, resource, opts)
      else
        {matches, non_matches} =
          results
          |> Enum.split_with(fn
            {:ok, result} ->
              result.__metadata__[metadata_key] in List.wrap(changes[index])

            _ ->
              false
          end)

        match_records = matches

        matches =
          Enum.map(matches, fn match ->
            {changesets_by_index[match.__metadata__[metadata_key]], match}
          end)

        after_batch_results =
          module.after_batch(
            matches,
            change_opts,
            struct(
              Ash.Resource.Change.Context,
              %{
                bulk?: true,
                actor: opts[:actor],
                tenant: opts[:tenant],
                tracer: opts[:tracer],
                authorize?: opts[:authorize?]
              }
            )
          )
          |> handle_after_batch_results(match_records, ref, resource, opts)

        Enum.concat([after_batch_results, non_matches])
      end
    end)
  end

  defp handle_after_batch_results(:ok, matches, _ref, _resource, _options), do: matches

  defp handle_after_batch_results(results, _matches, ref, resource, opts) do
    Enum.flat_map(
      results,
      fn
        %Ash.Notifier.Notification{} = notification ->
          store_notification(ref, notification, opts)

        {:ok, result} ->
          [result]

        {:error, error} ->
          if opts[:transaction] && opts[:rollback_on_error?] do
            if Ash.DataLayer.in_transaction?(resource) do
              Ash.DataLayer.rollback(
                resource,
                error
              )
            end
          end

          store_error(ref, error, opts)
          []
      end
    )
  end

  defp notification(changeset, result, opts) do
    %Ash.Notifier.Notification{
      resource: changeset.resource,
      domain: changeset.domain,
      actor: opts[:actor],
      for: Ash.Resource.Info.notifiers(changeset.resource) ++ changeset.action.notifiers,
      action: changeset.action,
      data: result,
      changeset: changeset
    }
  end

  defp run_action_changes(
         batch,
         all_changes,
         _action,
         actor,
         authorize?,
         tracer,
         tenant,
         context_key
       ) do
    context = %{
      actor: actor,
      authorize?: authorize? || false,
      tracer: tracer,
      tenant: tenant
    }

    Enum.reduce(
      all_changes,
      %{must_return_records?: false, batch: batch, changes: %{}},
      fn
        {%{validation: {module, opts}} = validation, _change_index}, %{batch: batch} = state ->
          batch =
            Enum.map(batch, fn changeset ->
              cond do
                validation.only_when_valid? && !changeset.valid? ->
                  changeset

                Enum.all?(validation.where || [], fn {module, opts} ->
                  opts = templated_opts(opts, actor, changeset.arguments, changeset.context)
                  {:ok, opts} = module.init(opts)

                  module.validate(
                    changeset,
                    opts,
                    struct(Ash.Resource.Validation.Context, context)
                  ) == :ok
                end) ->
                  opts = templated_opts(opts, actor, changeset.arguments, changeset.context)
                  {:ok, opts} = module.init(opts)

                  case module.validate(
                         changeset,
                         opts,
                         struct(
                           Ash.Resource.Validation.Context,
                           Map.put(context, :message, validation.message)
                         )
                       ) do
                    :ok ->
                      changeset

                    {:error, error} ->
                      Ash.Changeset.add_error(changeset, error)
                  end

                true ->
                  changeset
              end
            end)

          %{
            state
            | must_return_records?: state.must_return_records?,
              batch: batch,
              changes: state.changes
          }

        {%{change: {module, change_opts}} = change, change_index}, %{batch: batch} = state ->
          # could track if any element in the batch is invalid, and if not use the fast version
          if Enum.empty?(change.where) && !change.only_when_valid? do
            batch = batch_change(module, batch, change_opts, context, actor)

            must_return_records? =
              state.must_return_records? ||
                Enum.any?(batch, fn item ->
                  item.relationships not in [nil, %{}] || !Enum.empty?(item.after_action)
                end) ||
                (function_exported?(module, :after_batch, 3) &&
                   function_exported?(module, :batch_change, 3) &&
                   module.batch_callbacks?(batch, change_opts, context))

            %{
              state
              | must_return_records?: must_return_records?,
                batch: batch,
                changes: Map.put(state.changes, change_index, :all)
            }
          else
            {matches, non_matches} =
              batch
              |> Enum.split_with(fn changeset ->
                applies_from_where? =
                  Enum.all?(change.where || [], fn {module, opts} ->
                    opts = templated_opts(opts, actor, changeset.arguments, changeset.context)
                    {:ok, opts} = module.init(opts)

                    module.validate(
                      changeset,
                      opts,
                      struct(Ash.Resource.Validation.Context, context)
                    ) == :ok
                  end)

                applies_from_only_when_valid? =
                  if change.only_when_valid? do
                    changeset.valid?
                  else
                    true
                  end

                applies_from_where? and applies_from_only_when_valid?
              end)

            if Enum.empty?(matches) do
              %{
                state
                | must_return_records?: state.must_return_records?,
                  batch: non_matches,
                  changes: state.changes
              }
            else
              matches = batch_change(module, matches, change_opts, context, actor)

              must_return_records? =
                state.must_return_records? ||
                  Enum.any?(batch, fn item ->
                    item.relationships not in [nil, %{}] || !Enum.empty?(item.after_action)
                  end) ||
                  (function_exported?(module, :after_batch, 3) &&
                     function_exported?(module, :batch_change, 3) &&
                     module.batch_callbacks?(batch, change_opts, context))

              %{
                state
                | must_return_records?: must_return_records?,
                  batch: Enum.concat(matches, non_matches),
                  changes:
                    Map.put(
                      state.changes,
                      change_index,
                      Enum.map(matches, & &1.context[context_key].index)
                    )
              }
            end
          end
      end
    )
  end

  defp batch_change(module, batch, change_opts, context, actor) do
    case change_opts do
      {:templated, change_opts} ->
        if function_exported?(module, :batch_change, 3) do
          {:ok, change_opts} = module.init(change_opts)
          module.batch_change(batch, change_opts, context)
        else
          Enum.map(batch, fn changeset ->
            {:ok, change_opts} = module.init(change_opts)

            module.change(
              changeset,
              change_opts,
              struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
            )
          end)
        end

      change_opts ->
        Enum.map(batch, fn changeset ->
          change_opts = templated_opts(change_opts, actor, changeset.arguments, changeset.context)
          {:ok, change_opts} = module.init(change_opts)

          module.change(
            changeset,
            change_opts,
            struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
          )
        end)
    end
  end

  defp templated_opts({:templated, opts}, _actor, _arguments, _context), do: opts

  defp templated_opts(opts, actor, arguments, context) do
    Ash.Expr.fill_template(
      opts,
      actor,
      arguments,
      context
    )
  end

  defp get_read_action(resource, opts) do
    case opts[:read_action] do
      nil ->
        Ash.Resource.Info.primary_action!(resource, :read)

      action ->
        Ash.Resource.Info.action(resource, action)
    end
  end
end
