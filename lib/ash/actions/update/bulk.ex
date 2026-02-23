# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

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

    {query, opts} =
      if query.__validated_for_action__ do
        {query, opts}
      else
        {query, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, query, opts)

        query =
          Ash.Query.for_read(
            query,
            get_read_action(query.resource, action, opts).name,
            %{},
            actor: opts[:actor],
            tenant: opts[:tenant],
            context: %{query_for: :bulk_update}
          )

        {query, opts}
      end

    query = %{query | domain: domain}

    fully_atomic_changeset =
      cond do
        not_atomic_reason ->
          {:not_atomic, not_atomic_reason}

        :atomic not in opts[:strategy] ->
          {:not_atomic, "Not in requested strategies"}

        query.action.manual ->
          {:not_atomic, "Manual read actions cannot be updated atomically"}

        !Enum.empty?(query.before_action) ->
          {:not_atomic, "cannot atomically update a query if it has `before_action` hooks"}

        !Enum.empty?(query.after_action) ->
          {:not_atomic, "cannot atomically update a query if it has `after_action` hooks"}

        changeset = opts[:atomic_changeset] ->
          changeset

        Ash.DataLayer.data_layer_can?(query.resource, :update_query) ->
          private_context = Map.new(Keyword.take(opts, [:actor, :tenant, :authorize]))

          opts =
            Keyword.update(
              opts,
              :context,
              %{private: private_context},
              fn context ->
                Map.update(context, :private, private_context, &Map.merge(&1, private_context))
              end
            )

          Ash.Changeset.fully_atomic_changeset(query.resource, action, input, opts)

        true ->
          {:not_atomic, "data layer does not support updating a query"}
      end

    case fully_atomic_changeset do
      {:not_atomic, reason} ->
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

            read_opts =
              opts
              |> then(fn read_opts ->
                if opts[:stream_batch_size] do
                  Keyword.put(read_opts, :batch_size, opts[:stream_batch_size])
                else
                  Keyword.delete(read_opts, :batch_size)
                end
              end)
              |> Keyword.put(:authorize?, opts[:authorize?] && opts[:authorize_query?])
              |> Keyword.update(
                :context,
                %{query_for: :bulk_update},
                &Map.put(&1, :query_for, :bulk_update)
              )
              |> Keyword.put(:domain, domain)
              |> Keyword.delete(:load)

            if query.limit && query.limit < (opts[:batch_size] || 100) do
              read_opts = Keyword.take(read_opts, Keyword.keys(Ash.read_opts()))

              case Ash.Actions.Read.unpaginated_read(query, query.action, read_opts) do
                {:ok, results} ->
                  run(
                    domain,
                    results,
                    action,
                    input,
                    Keyword.merge(
                      opts,
                      [
                        resource: query.resource,
                        input_was_stream?: false,
                        context:
                          opts[:atomic_changeset] &&
                            Map.delete(opts[:atomic_changeset].context, :private)
                      ],
                      fn
                        :context, v1, v2 ->
                          Ash.Helpers.deep_merge_maps(v1, v2)

                        _k, _v1, v2 ->
                          v2
                      end
                    ),
                    reason
                  )

                {:error, error} ->
                  %Ash.BulkResult{
                    status: :error,
                    error_count: 1,
                    errors: [Ash.Error.to_error_class(error)]
                  }
              end
            else
              read_opts =
                Keyword.put(
                  Keyword.take(read_opts, Ash.stream_opt_keys()),
                  :batch_size,
                  opts[:batch_size] || 100
                )

              # We need to figure out a way to capture errors raised by the stream when picking items off somehow
              # for now, we only go this route if there are potentially more records in the result set than
              # in the batch size, to solve this problem for atomic upgrades.
              # we can likely make the stream throw something instead of raising something
              # like `{:stream_error, ...}` if a specific option is passed in.
              # once we figure this out, we may be able to remove the branch above
              run(
                domain,
                Ash.stream!(
                  query,
                  read_opts
                ),
                action,
                input,
                Keyword.merge(
                  opts,
                  [
                    resource: query.resource,
                    input_was_stream?: false,
                    query_sort: query.sort,
                    context:
                      opts[:atomic_changeset] &&
                        Map.delete(opts[:atomic_changeset].context, :private)
                  ],
                  fn
                    :context, v1, v2 ->
                      Ash.Helpers.deep_merge_maps(v1, v2)

                    _k, _v1, v2 ->
                      v2
                  end
                ),
                reason
              )
            end
        end

      %Ash.Changeset{valid?: false, errors: errors} = changeset ->
        # Run after_transaction hooks for failed changesets
        case Ash.Changeset.run_after_transactions(
               {:error, Ash.Error.to_error_class(errors, changeset: changeset)},
               changeset
             ) do
          {:ok, result} ->
            %Ash.BulkResult{
              status: :success,
              records: [result]
            }

          {:error, error} ->
            %Ash.BulkResult{
              status: :error,
              error_count: 1,
              errors: [error]
            }
        end

      atomic_changeset ->
        atomic_changeset =
          Ash.Changeset.filter(atomic_changeset, opts[:filter])

        {atomic_changeset, opts} =
          Ash.Actions.Helpers.set_context_and_get_opts(domain, atomic_changeset, opts)

        atomic_changeset = Ash.Actions.Helpers.apply_opts_load(atomic_changeset, opts)

        atomic_changeset =
          if opts[:select] do
            Ash.Changeset.select(atomic_changeset, opts[:select])
          else
            atomic_changeset
          end

        atomic_changeset = %{atomic_changeset | domain: domain}

        notify? = !Process.put(:ash_started_transaction?, true)

        try do
          context =
            struct(
              Ash.Resource.Change.Context,
              %{
                bulk?: true,
                source_context: atomic_changeset.context,
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
                  module.has_after_batch?() &&
                    module.batch_callbacks?(query, change_opts, context)

                _ ->
                  false
              end
            )

          {context_key, metadata_key} =
            case atomic_changeset.action.type do
              :update ->
                {:bulk_update, :bulk_update_index}

              :destroy ->
                {:bulk_destroy, :bulk_destroy_index}
            end

          prefer_transaction? =
            has_after_batch_hooks? || !Enum.empty?(atomic_changeset.after_action) ||
              Ash.DataLayer.prefer_transaction_for_atomic_updates?(atomic_changeset.resource)

          result =
            if prefer_transaction? &&
                 Keyword.get(opts, :transaction, true) do
              Ash.DataLayer.in_transaction?(atomic_changeset.resource)

              Ash.DataLayer.transaction(
                atomic_changeset.resource,
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
                },
                rollback_on_error?: false
              )
            else
              {:ok,
               do_atomic_update(query, atomic_changeset, has_after_batch_hooks?, input, opts)}
            end

          # Normalize result - either BulkResult (from fallback) or tagged tuples
          result
          |> case do
            {:ok, %Ash.BulkResult{} = bulk_result} ->
              bulk_result

            {:ok, {:ok, tagged_results, notifications}} ->
              {tagged_results, notifications}

            {:ok, {:error, error}} ->
              {[{:error, error, atomic_changeset}], []}

            {:error, error} ->
              {[{:error, error, atomic_changeset}], []}
          end
          |> case do
            %Ash.BulkResult{} = bulk_result ->
              bulk_result

            {tagged_results, notifications} ->
              ref = make_ref()

              try do
                processed =
                  tagged_results
                  |> process_results(
                    opts,
                    ref,
                    context_key,
                    metadata_key,
                    atomic_changeset.resource,
                    atomic_changeset.domain,
                    atomic_changeset
                  )
                  |> Enum.to_list()

                any_success? = Process.get({:any_success?, ref}, false)

                # Merge notifications from do_atomic_update with those from process_results
                all_notifications =
                  notifications ++ (Process.delete({:bulk_notifications, ref}) || [])

                Ash.Actions.BulkManualActionHelpers.build_bulk_result(
                  processed,
                  any_success?,
                  all_notifications,
                  opts
                )
              catch
                {:error, error} ->
                  status =
                    if Process.get({:any_success?, ref}) do
                      :partial_success
                    else
                      :error
                    end

                  all_notifications =
                    notifications ++ (Process.delete({:bulk_notifications, ref}) || [])

                  result = %Ash.BulkResult{
                    status: status,
                    records: [],
                    notifications: all_notifications
                  }

                  {error_count, errors} = Ash.Actions.Helpers.Bulk.errors(result, error, opts)

                  %{result | errors: errors, error_count: error_count}
              end
          end
          |> handle_atomic_notifications(atomic_changeset.resource, action, notify?, opts)
        after
          if notify? do
            Process.delete(:ash_started_transaction?)
          end
        end
    end
  rescue
    e ->
      action_name =
        case action do
          %{name: name} -> name
          name -> name
        end

      reraise Ash.Error.to_error_class(e,
                stacktrace: __STACKTRACE__,
                bread_crumbs: [
                  "Exception raised in bulk update: #{inspect(opts[:resource])}.#{action_name}"
                ]
              ),
              __STACKTRACE__
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

    {context_key, metadata_key, ref_metadata_key} =
      case action.type do
        :destroy ->
          {:bulk_destroy, :bulk_destroy_index, :bulk_action_ref}

        _ ->
          {:bulk_update, :bulk_update_index, :bulk_action_ref}
      end

    if opts[:transaction] == :all &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      notify? = !Process.put(:ash_started_transaction?, true)

      try do
        Ash.DataLayer.transaction(
          List.wrap(resource) ++ action.touches_resources,
          fn ->
            do_run(
              domain,
              stream,
              action,
              input,
              Keyword.merge(opts,
                notify?: opts[:notify?],
                notification_metadata: opts[:notification_metadata],
                return_notifications?: opts[:notify?]
              ),
              metadata_key,
              ref_metadata_key,
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
          },
          rollback_on_error?: false
        )
        |> case do
          {:ok, bulk_result} ->
            bulk_result =
              if notify? do
                notifications =
                  if opts[:return_notifications?] do
                    bulk_result.notifications ++ List.wrap(Process.delete(:ash_notifications))
                  else
                    if opts[:notify?] do
                      remaining_notifications =
                        Ash.Notifier.notify(
                          bulk_result.notifications ++
                            List.wrap(Process.delete(:ash_notifications))
                        )

                      Ash.Actions.Helpers.warn_missed!(resource, action, %{
                        resource_notifications: remaining_notifications
                      })
                    else
                      []
                    end
                  end

                %{
                  bulk_result
                  | notifications: notifications
                }
              else
                Process.put(
                  :ash_notifications,
                  List.wrap(Process.get(:ash_notifications)) ++
                    List.wrap(bulk_result.notifications)
                )

                bulk_result
              end

            handle_bulk_result(bulk_result, metadata_key, opts)

          {:error, error} ->
            handle_bulk_result(
              %Ash.BulkResult{errors: [error], status: :error},
              metadata_key,
              opts
            )
        end
      after
        if notify? do
          Process.delete(:ash_started_transaction?)
        end
      end
    else
      domain
      |> do_run(
        stream,
        action,
        input,
        opts,
        metadata_key,
        ref_metadata_key,
        context_key,
        not_atomic_reason
      )
      |> handle_bulk_result(metadata_key, opts)
    end
  rescue
    e ->
      action_name =
        case action do
          %{name: name} -> name
          name -> name
        end

      reraise Ash.Error.to_error_class(e,
                stacktrace: __STACKTRACE__,
                bread_crumbs: [
                  "Exception raised in bulk update: #{inspect(opts[:resource])}.#{action_name}"
                ]
              ),
              __STACKTRACE__
  end

  defp do_atomic_update(query, atomic_changeset, has_after_batch_hooks?, input, opts) do
    context =
      struct(
        Ash.Resource.Change.Context,
        %{
          bulk?: true,
          source_context: atomic_changeset.context,
          actor: opts[:actor],
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        }
      )

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

    action_select =
      if Ash.DataLayer.data_layer_can?(atomic_changeset.resource, :action_select) do
        Enum.uniq(
          Enum.concat(
            Ash.Resource.Info.action_select(atomic_changeset.resource, atomic_changeset.action),
            List.wrap(
              opts[:select] ||
                MapSet.to_list(
                  Ash.Resource.Info.selected_by_default_attribute_names(atomic_changeset.resource)
                )
            )
          )
        )
      else
        MapSet.to_list(Ash.Resource.Info.attribute_names(atomic_changeset.resource))
      end

    return_records? =
      has_after_batch_hooks? || opts[:notify?] || opts[:return_records?] ||
        !Enum.empty?(atomic_changeset.after_action) ||
        !Enum.empty?(atomic_changeset.after_transaction)

    # Update opts with calculated return_records? so handle_bulk_result uses it
    opts = Keyword.put(opts, :return_records?, return_records?)

    update_query_opts =
      opts
      |> Keyword.take([:tenant])
      |> Map.new()
      |> Map.put(:return_records?, return_records?)
      |> Map.put(:calculations, calculations)
      |> Map.put(
        :action_select,
        action_select
      )

    with :ok <-
           Ash.Actions.Helpers.Bulk.validate_multitenancy(
             atomic_changeset.resource,
             atomic_changeset.action,
             opts
           ),
         {:ok, query} <-
           authorize_bulk_query(query, atomic_changeset, opts),
         {:ok, atomic_changeset, query} <-
           authorize_atomic_changeset(query, atomic_changeset, opts),
         {query, atomic_changeset} <-
           add_changeset_filters(query, atomic_changeset),
         query =
           Ash.Actions.Read.add_calc_context_to_query(
             query,
             opts[:actor],
             opts[:authorize?],
             query.tenant,
             opts[:tracer],
             query.domain,
             expand?: true,
             source_context: query.context
           ),
         %Ash.Changeset{valid?: true} = atomic_changeset <-
           Ash.Changeset.handle_allow_nil_atomics(atomic_changeset, opts[:actor]),
         atomic_changeset <- sort_atomic_changes(atomic_changeset),
         {:ok, query} <- Ash.Actions.Read.handle_multitenancy(query),
         {:ok, data_layer_query} <-
           Ash.Query.data_layer_query(query) do
      update_query_result =
        if atomic_changeset.context.changed? do
          Ash.DataLayer.update_query(
            data_layer_query,
            atomic_changeset,
            update_query_opts
          )
        else
          atomic_validation_expr =
            with [field | _] <- Ash.Resource.Info.primary_key(atomic_changeset.resource),
                 {:ok, v} <- Keyword.fetch(atomic_changeset.atomics, field) do
              v
            else
              _ ->
                nil
            end

          if is_nil(atomic_validation_expr) && is_nil(query.filter) &&
               !update_query_opts[:return_records?] do
            :ok
          else
            Ash.DataLayer.update_query(
              data_layer_query,
              atomic_changeset,
              update_query_opts
            )
          end
        end

      case update_query_result do
        :ok ->
          {:ok, [], []}

        {:ok, results} ->
          results =
            Ash.Actions.Helpers.select(results, %{
              resource: query.resource,
              select: action_select
            })

          results =
            case results do
              [result] ->
                if atomic_changeset.context[:data_layer][:use_atomic_update_data?] do
                  Map.put(
                    result,
                    :__metadata__,
                    Map.merge(atomic_changeset.data.__metadata__, result.__metadata__)
                  )
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

          {results, errors, notifications} =
            if Enum.empty?(atomic_changeset.after_action) do
              {results, [], notifications}
            else
              Enum.reduce(
                results,
                {[], [], notifications},
                fn result, {results, errors, notifications} ->
                  # we can't actually know if the changeset changed or not when doing atomics
                  # so we just have to set it to statically true here.

                  case Ash.Changeset.run_after_actions(result, atomic_changeset, []) do
                    {:error, error} ->
                      if opts[:transaction] && opts[:rollback_on_error?] do
                        if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
                          Ash.DataLayer.rollback(
                            atomic_changeset.resource,
                            error
                          )
                        end
                      end

                      {results, errors ++ List.wrap(error), notifications}

                    {:ok, result, _changeset, %{notifications: more_new_notifications}} ->
                      {[result | results], errors, notifications ++ more_new_notifications}
                  end
                end
              )
              |> then(fn {results, errors, notifications} ->
                {Enum.reverse(results), errors, notifications}
              end)
            end

          # Return tagged tuples for process_results to handle after_transaction hooks.
          # This ensures errors are counted exactly once in build_bulk_result.
          tagged_results =
            Enum.map(results, &{:ok, &1, atomic_changeset}) ++
              Enum.map(errors, &{:error, &1, atomic_changeset})

          {:ok, tagged_results, notifications}

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

          {:error,
           Ash.Error.to_error_class(error,
             bread_crumbs: [
               "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
             ]
           )}

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
            Ash.DataLayer.rollback(
              atomic_changeset.resource,
              Ash.Error.to_error_class(error,
                bread_crumbs: [
                  "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
                ]
              )
            )
          else
            {:error,
             Ash.Error.to_error_class(error,
               bread_crumbs: [
                 "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
               ]
             )}
          end

        {:error, :no_rollback, error} ->
          {:error,
           Ash.Error.to_error_class(error,
             bread_crumbs: [
               "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
             ]
           )}

        {:error, error} ->
          if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
            Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
          else
            {:error,
             Ash.Error.to_error_class(error,
               bread_crumbs: [
                 "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
               ]
             )}
          end
      end
    else
      %Ash.Changeset{valid?: false, errors: error} = _changeset ->
        if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
          Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
        else
          {:error,
           Ash.Error.to_error_class(error,
             bread_crumbs: [
               "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
             ]
           )}
        end

      {:error, %Ash.Error.Forbidden.InitialDataRequired{} = e} ->
        case Ash.Actions.Read.Stream.stream_strategy(
               query,
               nil,
               opts[:allow_stream_with] || :keyset
             ) do
          {:error, %Ash.Error.Invalid.NonStreamableAction{} = exception} ->
            {:error,
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
             )}

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
        {:error,
         Ash.Error.to_error_class(error,
           bread_crumbs: [
             "Returned from bulk query update: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
           ]
         )}
    end
  end

  defp handle_atomic_notifications(bulk_result, resource, action, notify?, opts) do
    if opts[:return_notifications?] do
      bulk_result
    else
      if notify? do
        notifications =
          List.wrap(Process.delete(:ash_notifications)) ++
            List.wrap(bulk_result.notifications)

        if opts[:notify?] do
          remaining = Ash.Notifier.notify(notifications)

          Ash.Actions.Helpers.warn_missed!(resource, action, %{
            resource_notifications: remaining
          })

          %{bulk_result | notifications: remaining}
        else
          %{bulk_result | notifications: []}
        end
      else
        process_notifications = List.wrap(Process.get(:ash_notifications, []))

        Process.put(
          :ash_notifications,
          process_notifications ++ List.wrap(bulk_result.notifications)
        )

        %{bulk_result | notifications: []}
      end
    end
  end

  defp sort_atomic_changes(atomic_changeset) do
    primary_key = Ash.Resource.Info.primary_key(atomic_changeset.resource)

    Map.update!(atomic_changeset, :atomics, fn atomics ->
      Enum.sort_by(atomics, fn {key, _} ->
        key not in primary_key
      end)
    end)
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
          module.has_after_batch?() &&
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
              atomic_changeset.domain,
              atomic_changeset.resource,
              source_context: atomic_changeset.context
            )

          [{calculation, where}]
      end)

    {all_changes, conditional_after_batch_hooks, calculations}
  end

  defp set_strategy(opts, resource, inputs_is_enumerable? \\ false) do
    opts =
      if Ash.DataLayer.data_layer_can?(resource, :update_query) do
        if Ash.DataLayer.data_layer_can?(resource, :expr_error) do
          opts
        else
          Keyword.put_new(opts, :strategy, [:stream, :atomic_batches, :atomic])
        end
      else
        Keyword.put(opts, :strategy, [:stream])
      end

    if inputs_is_enumerable? && :atomic in List.wrap(opts[:strategy]) do
      Keyword.put(opts, :strategy, Enum.uniq([:atomic_batches | List.wrap(opts[:strategy])]))
    else
      opts
    end
  end

  defp do_run(
         domain,
         stream,
         action,
         input,
         opts,
         metadata_key,
         ref_metadata_key,
         context_key,
         not_atomic_reason
       ) do
    resource = opts[:resource]
    opts = Ash.Actions.Helpers.set_opts(opts, domain)
    read_action = get_read_action(resource, action, opts)

    {context_cs, opts} =
      Ash.Actions.Helpers.set_context_and_get_opts(domain, Ash.Changeset.new(resource), opts)

    case Ash.Actions.Helpers.Bulk.validate_multitenancy(resource, action, opts) do
      {:error, error} ->
        %Ash.BulkResult{
          status: :error,
          error_count: 1,
          errors: [
            Ash.Error.to_error_class(error)
          ]
        }

      :ok ->
        fully_atomic_changeset =
          cond do
            :atomic_batches not in opts[:strategy] ->
              {:not_atomic, "Not in requested strategies"}

            Enum.empty?(Ash.Resource.Info.primary_key(resource)) ->
              {:not_atomic, "cannot atomically update a stream without a primary key"}

            !read_action ->
              {:not_atomic, "cannot atomically update a stream without a primary read action"}

            read_action.manual ->
              {:not_atomic, "Manual read actions cannot be updated atomically"}

            Ash.DataLayer.data_layer_can?(resource, :update_query) ->
              opts =
                Keyword.update(
                  opts,
                  :context,
                  %{private: context_cs.context.private},
                  &Map.put(&1 || %{}, :private, context_cs.context.private)
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
                  do_stream_batches(
                    domain,
                    stream,
                    action,
                    input,
                    opts,
                    metadata_key,
                    ref_metadata_key,
                    context_key
                  )
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
              do_stream_batches(
                domain,
                stream,
                action,
                input,
                opts,
                metadata_key,
                ref_metadata_key,
                context_key
              )
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
        pkeys = Ash.pkey_filter(batch, pkey)

        read_action = get_read_action(resource, action, opts).name

        resource
        |> Ash.Query.for_read(read_action, %{},
          actor: opts[:actor],
          authorize?: false,
          context: Map.put(atomic_changeset.context, :query_for, :bulk_update),
          tenant: atomic_changeset.tenant,
          tracer: opts[:tracer]
        )
        |> Ash.Query.set_context(%{private: %{internal?: true}})
        |> Ash.Query.filter(^pkeys)
        |> Ash.Query.filter(^atomic_changeset.filter)
        |> Ash.Query.select([])
        |> Ash.Query.sort(opts[:query_sort] || [])
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
            notification_metadata: opts[:notification_metadata],
            return_records?: opts[:return_records?],
            allow_stream_with: opts[:allow_stream_with],
            read_action: read_action,
            strategy: [:atomic],
            transaction: opts[:transaction] || true,
            rollback_on_error?: opts[:rollback_on_error?],
            stop_on_error?: opts[:stop_on_error?]
          )
          |> case do
            %Ash.BulkResult{
              error_count: 0,
              records: records,
              notifications: notifications,
              status: status
            } ->
              Ash.Actions.Helpers.Bulk.set_success(ref, status)

              Ash.Actions.Helpers.Bulk.store_notification(ref, notifications, opts)
              List.wrap(records)

            %Ash.BulkResult{
              errors: errors,
              notifications: notifications,
              error_count: error_count,
              status: status
            } ->
              Ash.Actions.Helpers.Bulk.set_success(ref, status)

              Ash.Actions.Helpers.Bulk.store_notification(ref, notifications, opts)

              if errors && errors != [] do
                Ash.Actions.Helpers.Bulk.maybe_stop_on_error(hd(errors), opts)
                Enum.map(errors, &{:error, &1})
              else
                Ash.Actions.Helpers.Bulk.maybe_stop_on_error(:transaction_error, opts)
                # No error details but we have a count - create placeholder tuples
                for _ <- 1..error_count, do: {:error, :transaction_error}
              end
          end
        end)
      end
    )
    |> run_batches(ref, atomic_changeset.resource, atomic_changeset.action.name, opts)
  end

  defp do_stream_batches(
         domain,
         stream,
         action,
         input,
         opts,
         metadata_key,
         ref_metadata_key,
         context_key
       ) do
    resource = opts[:resource]

    action_select =
      Enum.uniq(
        Enum.concat(
          Ash.Resource.Info.action_select(resource, action.name),
          List.wrap(
            opts[:select] ||
              MapSet.to_list(Ash.Resource.Info.selected_by_default_attribute_names(resource))
          )
        )
      )

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
      pre_template_all_changes(
        action,
        resource,
        action.type,
        base_changeset,
        opts[:actor],
        base_changeset.to_tenant
      )

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
            ref_metadata_key,
            base_changeset,
            action_select
          )
        after
          if opts[:return_stream?] && opts[:notify?] && !opts[:return_notifications?] do
            Ash.Notifier.notify(Process.delete({:bulk_notifications, ref}) || [])
          end
        end
      end
    )
    |> run_batches(ref, resource, action.name, opts)
  end

  defp run_batches(changeset_stream, ref, resource, action_name, opts) do
    if opts[:return_stream?] do
      Stream.concat(changeset_stream)
    else
      try do
        {records, error_tuples} =
          changeset_stream
          |> Stream.concat()
          |> Enum.split_with(fn
            {:error, _} -> false
            _ -> true
          end)

        records = if opts[:return_records?], do: records, else: nil

        notifications =
          if opts[:notify?] do
            Process.delete({:bulk_notifications, ref}) || []
          else
            []
          end

        # Unless return_notifications? is true, we always want to end up with nil.
        notifications =
          cond do
            Enum.empty?(notifications) ->
              if opts[:return_notifications?], do: [], else: nil

            opts[:return_notifications?] ->
              notifications

            true ->
              notifications =
                if opts[:notify?] do
                  Ash.Notifier.notify(notifications)
                else
                  notifications
                end

              if Process.get(:ash_started_transaction?) do
                Process.put(
                  :ash_notifications,
                  Process.get(:ash_notifications, []) ++ notifications
                )
              end

              nil
          end

        error_count = length(error_tuples)

        errors =
          if opts[:return_errors?] do
            Enum.map(error_tuples, fn {:error, error} ->
              Ash.Error.to_ash_error(error, [],
                bread_crumbs: [
                  "Returned from bulk update: #{inspect(resource)}.#{action_name}"
                ]
              )
            end)
          else
            nil
          end

        bulk_result = %Ash.BulkResult{
          records: records,
          errors: errors,
          notifications: notifications,
          error_count: error_count
        }

        case bulk_result do
          %{records: _, error_count: 0} ->
            %{bulk_result | status: :success}

          %{records: records, error_count: _} when records in [nil, []] ->
            %{bulk_result | status: :error}

          _ ->
            %{bulk_result | status: :partial_success}
        end
      catch
        {:error, error} ->
          status =
            if Process.get({:any_success?, ref}) do
              :partial_success
            else
              :error
            end

          accumulated_notifications = Process.delete({:bulk_notifications, ref})

          notifications =
            if opts[:notify?] do
              if opts[:return_notifications?] do
                accumulated_notifications
              else
                Ash.Notifier.notify(accumulated_notifications || [])
              end
            end

          result = %Ash.BulkResult{
            status: status,
            notifications: notifications
          }

          {error_count, errors} = Ash.Actions.Helpers.Bulk.errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_notifications, ref})
        Process.delete({:any_success?, ref})
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
             filter_with: opts[:authorize_query_with] || opts[:authorize_with] || :filter,
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
             filter_with: opts[:authorize_changeset_with] || opts[:authorize_with] || :filter,
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
    |> Ash.Changeset.set_private_arguments_for_action(opts[:private_arguments] || %{})
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

  defp pre_template_all_changes(action, resource, _type, base, actor, tenant) do
    action.changes
    |> then(fn changes ->
      if action.skip_global_validations? do
        changes
      else
        Enum.concat(changes, Ash.Resource.Info.validations(resource, action.type))
      end
    end)
    |> Enum.concat(Ash.Resource.Info.changes(resource, action.type))
    |> Enum.map(fn
      %{change: {module, opts}} = change ->
        %{change | change: {module, pre_template(opts, base, actor, tenant)}}

      %{validation: {module, opts}} = validation ->
        %{validation | validation: {module, pre_template(opts, base, actor, tenant)}}
    end)
    |> Enum.map(fn
      %{where: where} = change ->
        new_where =
          if where do
            where
            |> List.wrap()
            |> Enum.map(fn {module, opts} ->
              {module, pre_template(opts, base, actor, tenant)}
            end)
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

  defp pre_template(opts, changeset, actor, tenant) do
    if Ash.Expr.template_references_context?(opts) do
      opts
    else
      {:templated,
       Ash.Expr.fill_template(
         opts,
         actor: actor,
         tenant: tenant,
         args: changeset.arguments,
         context: changeset.context,
         changeset: changeset
       )}
    end
  end

  defp notification_stream(ref) do
    the_notifications = Process.delete({:bulk_notifications, ref})

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
         ref_metadata_key,
         base_changeset,
         action_select
       ) do
    %{
      must_return_records?: must_return_records_for_changes?,
      batch: batch,
      re_sort?: re_sort?,
      changes: changes
    } =
      run_action_changes(
        batch,
        all_changes,
        action,
        resource,
        opts[:actor],
        opts[:authorize?],
        opts[:tracer],
        opts[:tenant],
        context_key
      )

    batch = authorize(batch, opts)

    batch =
      if re_sort? do
        Enum.sort_by(batch, & &1.context[context_key].index)
      else
        batch
      end

    {batch, must_be_simple_results} =
      Ash.Actions.Helpers.split_and_run_simple(
        batch,
        action,
        opts,
        changes,
        all_changes,
        context_key,
        fn changeset ->
          case Ash.Actions.Update.run(
                 domain,
                 changeset,
                 action,
                 Keyword.merge(opts,
                   atomic_upgrade?: false,
                   return_destroyed?: opts[:return_records?]
                 )
               ) do
            :ok ->
              []

            {:ok, result} ->
              [
                Ash.Resource.set_metadata(result, %{
                  metadata_key => changeset.context |> Map.get(context_key) |> Map.get(:index),
                  ref_metadata_key => changeset.context |> Map.get(context_key) |> Map.get(:ref)
                })
              ]

            {:ok, result, notifications} ->
              Ash.Actions.Helpers.Bulk.store_notification(ref, notifications, opts)

              [
                Ash.Resource.set_metadata(result, %{
                  metadata_key => changeset.context |> Map.get(context_key) |> Map.get(:index),
                  ref_metadata_key => changeset.context |> Map.get(context_key) |> Map.get(:ref)
                })
              ]

            {:error, error} ->
              error
              |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
              |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
              |> Ash.Helpers.error()
              |> List.wrap()
          end
        end
      )

    # Separate valid and invalid changesets
    # Run after_transaction hooks immediately for invalid changesets to determine
    # if they convert errors to success. This allows stop_on_error? to work correctly
    # while still honoring hooks that convert errors.
    # Results are tagged with :ok_hooks_done or :error_hooks_done so process_results
    # knows not to run after_transaction hooks again.
    {batch, invalid_changeset_results} =
      Enum.reduce(batch, {[], []}, fn
        %{valid?: false} = changeset, {batch_acc, results_acc} ->
          error = Ash.Error.to_error_class(changeset.errors, changeset: changeset)

          case Ash.Changeset.run_after_transactions({:error, error}, changeset) do
            {:ok, result} ->
              {batch_acc, [{:ok_hooks_done, result, changeset} | results_acc]}

            {:error, error} ->
              error
              |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
              |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)

              {batch_acc, [{:error_hooks_done, error, changeset} | results_acc]}
          end

        changeset, {batch_acc, results_acc} ->
          {[changeset | batch_acc], results_acc}
      end)

    batch = Enum.reverse(batch)
    invalid_changeset_results = Enum.reverse(invalid_changeset_results)

    if opts[:transaction] == :batch &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      context = batch |> Enum.at(0) |> Kernel.||(%{}) |> Map.get(:context)

      notify? = opts[:notify?] && !Process.put(:ash_started_transaction?, true)

      try do
        Ash.DataLayer.transaction(
          List.wrap(resource) ++ action.touches_resources,
          fn ->
            do_handle_batch(
              batch,
              domain,
              resource,
              action,
              opts,
              all_changes,
              ref,
              metadata_key,
              ref_metadata_key,
              context_key,
              must_return_records_for_changes?,
              changes,
              action_select
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
            data_layer_context: opts[:data_layer_context] || context
          },
          rollback_on_error?: false
        )
        |> case do
          {:ok, tagged_results} ->
            # process_results runs OUTSIDE transaction - after_transaction hooks run here
            # Include invalid_changeset_results so they go through the after_transactions
            all_tagged_results = invalid_changeset_results ++ tagged_results

            process_results(
              all_tagged_results,
              opts,
              ref,
              context_key,
              metadata_key,
              resource,
              domain,
              base_changeset
            )
            |> Stream.concat(must_be_simple_results)
            |> then(fn stream ->
              if opts[:return_stream?] do
                stream
                |> Stream.map(fn
                  {:error, _} = error_tuple -> error_tuple
                  record -> {:ok, clear_ref_metadata(record)}
                end)
                |> Stream.concat(notification_stream(ref))
              else
                stream
              end
            end)

          {:error, error} ->
            # Convert batch changesets to error tuples for after_transaction processing
            error_tagged_results =
              Enum.map(batch, fn changeset ->
                {:error, error, changeset}
              end)

            all_tagged_results = invalid_changeset_results ++ error_tagged_results

            process_results(
              all_tagged_results,
              opts,
              ref,
              context_key,
              metadata_key,
              resource,
              domain,
              base_changeset
            )
            |> Stream.concat(must_be_simple_results)
            |> then(fn stream ->
              if opts[:return_stream?] do
                stream
                |> Stream.map(fn
                  {:error, _} = error_tuple -> error_tuple
                  record -> {:ok, clear_ref_metadata(record)}
                end)
                |> Stream.concat(notification_stream(ref))
              else
                stream
              end
            end)
        end
      after
        if notify? do
          Process.delete(:ash_started_transaction?)
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
        ref_metadata_key,
        context_key,
        must_return_records_for_changes?,
        changes,
        action_select
      )
      |> then(fn tagged_results ->
        # Include invalid_changeset_results so they go through the after_transactions
        invalid_changeset_results ++ tagged_results
      end)
      |> process_results(
        opts,
        ref,
        context_key,
        metadata_key,
        resource,
        domain,
        base_changeset
      )
      |> Stream.concat(must_be_simple_results)
      |> then(fn stream ->
        if opts[:return_stream?] do
          stream
          |> Stream.map(fn
            {:error, _} = error_tuple -> error_tuple
            record -> {:ok, clear_ref_metadata(record)}
          end)
          |> Stream.concat(notification_stream(ref))
        else
          stream
        end
      end)
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
         ref_metadata_key,
         context_key,
         must_return_records_for_changes?,
         changes,
         action_select
       ) do
    must_return_records? =
      opts[:notify?] ||
        Enum.any?(batch, fn item ->
          item.after_action != [] ||
            item.after_transaction != []
        end)

    # Can return both valid and invalid changesets
    batch =
      run_bulk_before_batches(
        batch,
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
      ref_metadata_key,
      context_key,
      action_select
    )
    |> run_after_action_hooks(opts, domain, ref)
    |> then(
      &run_bulk_after_changes(
        changes,
        all_changes,
        &1,
        batch,
        opts,
        ref,
        resource,
        metadata_key
      )
    )
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
    # Generate unique ref per changeset (not per batch)
    ref = make_ref()

    record
    |> Ash.Changeset.new()
    |> Map.put(:domain, domain)
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
    |> Ash.Changeset.set_private_arguments_for_action(opts[:private_arguments] || %{})
    |> Ash.Changeset.put_context(context_key, %{index: index, ref: ref})
    |> Ash.Changeset.set_context(opts[:context] || %{})
    |> Ash.Changeset.atomic_update(opts[:atomic_update] || [])
    |> Ash.Changeset.hydrate_atomic_refs(opts[:actor], opts)
    |> handle_params(
      Keyword.get(opts, :assume_casted?, false),
      action,
      opts,
      input,
      argument_names
    )
    |> Ash.Changeset.apply_atomic_constraints(opts[:actor])
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
      ash_context = Ash.ProcessHelpers.get_context_for_transfer(opts)

      Task.async_stream(
        stream,
        fn batch ->
          Ash.ProcessHelpers.transfer_context(ash_context, opts)
          Process.put(:ash_started_transaction?, true)
          batch_result = callback.(batch)

          notifications =
            if opts[:notify?] do
              process_notifications = Process.get(:ash_notifications, [])
              bulk_notifications = Process.get({:bulk_notifications, ref}) || []

              if opts[:return_notifications?] do
                process_notifications ++ bulk_notifications
              else
                if opts[:transaction] && opts[:transaction] != :all do
                  Ash.Notifier.notify(
                    Enum.concat(List.wrap(bulk_notifications), List.wrap(process_notifications))
                  )
                end

                []
              end
            end

          {batch_result, notifications, Process.get({:any_success?, ref})}
        end,
        timeout: :infinity,
        max_concurrency: max_concurrency
      )
      |> Stream.flat_map(fn
        {:ok, {:throw, value}} ->
          throw(value)

        {:ok,
         {%Ash.BulkResult{
            records: records,
            notifications: notifications,
            errors: errors,
            error_count: error_count
          }, _, any_success?}} ->
          Process.put({:any_success?, ref}, any_success?)
          Ash.Actions.Helpers.Bulk.store_notification(ref, notifications, opts)

          error_tuples =
            cond do
              errors && errors != [] ->
                if opts[:stop_on_error?] && !opts[:return_stream?] do
                  throw({:error, Ash.Error.to_error_class(hd(errors))})
                end

                Enum.map(errors, &{:error, &1})

              error_count > 0 ->
                if opts[:stop_on_error?] && !opts[:return_stream?] do
                  throw({:error, Ash.Error.to_error_class(:transaction_error)})
                end

                for _ <- 1..error_count, do: {:error, :transaction_error}

              true ->
                []
            end

          List.wrap(records) ++ error_tuples

        {:ok, {result, notifications, any_success?}} ->
          Process.put({:any_success?, ref}, any_success?)
          Ash.Actions.Helpers.Bulk.store_notification(ref, notifications, opts)
          # result already contains records and {:error, _} tuples inline
          List.wrap(result)

        {:exit, error} ->
          error
          |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
          |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
          |> Ash.Helpers.error()
          |> List.wrap()
      end)
    else
      Stream.map(stream, callback)
    end
  end

  defp index_changesets(batch, context_key) do
    Enum.reduce(batch, {%{}, %{}}, fn changeset, {by_ref, by_index} ->
      ref = changeset.context[context_key].ref
      index = changeset.context[context_key].index

      {
        Map.put(by_ref, ref, changeset),
        Map.put(by_index, index, ref)
      }
    end)
  end

  @doc false

  def run_bulk_before_batches(
        [],
        _changes,
        _all_changes,
        _opts,
        _ref,
        _context_key
      ) do
    []
  end

  def run_bulk_before_batches(
        batch,
        changes,
        all_changes,
        opts,
        ref,
        context_key
      ) do
    all_changes
    |> Enum.reduce(batch, fn
      {%{validation: _}, _index}, batch ->
        batch

      {%{change: {module, change_opts}}, index}, batch ->
        # change may return a stream but before_batch/3 expects a list
        batch = Enum.to_list(batch)

        context =
          struct(Ash.Resource.Change.Context, %{
            bulk?: true,
            source_context: Enum.at(batch, 0).context,
            actor: opts[:actor],
            tenant: opts[:tenant],
            tracer: opts[:tracer],
            authorize?: opts[:authorize?]
          })

        if changes[index] == :all do
          case change_opts do
            {:templated, change_opts} ->
              if module.has_before_batch?() && module.has_batch_change?() &&
                   module.batch_callbacks?(batch, change_opts, context) do
                module.before_batch(
                  batch,
                  change_opts,
                  context
                )
              else
                batch
              end

            change_opts ->
              Enum.flat_map(batch, fn changeset ->
                if module.has_before_batch?() && module.has_batch_change?() &&
                     module.batch_callbacks?(batch, change_opts, context) do
                  change_opts =
                    templated_opts(
                      change_opts,
                      opts[:actor],
                      changeset.to_tenant,
                      changeset.arguments,
                      changeset.context,
                      changeset
                    )

                  module.before_batch(
                    [changeset],
                    change_opts,
                    context
                  )
                else
                  [changeset]
                end
              end)
          end
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
            case change_opts do
              {:templated, change_opts} ->
                if module.has_before_batch?() &&
                     module.has_batch_change?() &&
                     module.batch_callbacks?(matches, change_opts, context) do
                  module.before_batch(
                    matches,
                    change_opts,
                    context
                  )
                else
                  matches
                end

              change_opts ->
                Enum.flat_map(matches, fn changeset ->
                  change_opts =
                    templated_opts(
                      change_opts,
                      opts[:actor],
                      changeset.to_tenant,
                      changeset.arguments,
                      changeset.context,
                      changeset
                    )

                  if module.has_before_batch?() &&
                       module.has_batch_change?() &&
                       module.batch_callbacks?([changeset], change_opts, context) do
                    module.before_batch(
                      [changeset],
                      change_opts,
                      context
                    )
                  else
                    [changeset]
                  end
                end)
            end

          Enum.concat([before_batch_results, non_matches])
        end
    end)
    |> Enum.reject(fn
      %Ash.Notifier.Notification{} = notification ->
        Ash.Actions.Helpers.Bulk.store_notification(ref, notification, opts)
        true

      _changeset ->
        false
    end)
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
    |> clear_ref_metadata_from_records()
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

  defp sort(%{records: records} = result, metadata_key, opts)
       when is_list(records) and not is_nil(metadata_key) do
    if opts[:sorted?] do
      %{result | records: Enum.sort_by(records, & &1.__metadata__[metadata_key])}
    else
      result
    end
  end

  defp sort(result, _metadata_key, _opts), do: result

  @spec run_batch(
          resource :: Ash.Resource.t(),
          batch :: [Ash.Changeset.t()],
          action :: Ash.Resource.Actions.action(),
          opts :: keyword(),
          must_return_records? :: boolean(),
          must_return_records_for_changes? :: boolean(),
          domain :: Ash.Domain.t(),
          ref :: reference(),
          metadata_key :: atom(),
          ref_metadata_key :: atom(),
          context_key :: atom(),
          action_select :: [atom()] | nil
        ) :: [Ash.Actions.Helpers.Bulk.tagged_result()]
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
         ref_metadata_key,
         context_key,
         action_select
       ) do
    batch =
      Enum.map(batch, fn changeset ->
        changeset =
          changeset
          |> Ash.Changeset.hydrate_atomic_refs(opts[:actor])
          |> Ash.Changeset.apply_atomic_constraints(opts[:actor])

        if changeset.valid? do
          {changeset, %{notifications: new_notifications}} =
            Ash.Changeset.run_before_actions(changeset)

          if !changeset.valid? && opts[:rollback_on_error?] do
            Ash.Actions.Helpers.rollback_if_in_transaction(
              {:error, changeset.errors},
              resource,
              nil
            )
          end

          changeset =
            changeset
            |> Ash.Changeset.handle_allow_nil_atomics(opts[:actor])
            |> Ash.Changeset.require_values(
              :update,
              true
            )
            |> Map.put(:action_select, action_select)

          changed? =
            Ash.Changeset.changing_attributes?(changeset) or
              not Enum.empty?(changeset.atomics)

          changeset =
            Ash.Changeset.put_context(
              changeset,
              :changed?,
              changeset.context[:changed?] || changed?
            )

          new_notifications =
            Ash.Actions.Helpers.Bulk.store_notification(ref, new_notifications, opts)

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

          Ash.Actions.Helpers.Bulk.store_notification(ref, manage_notifications, opts)

          changeset
        else
          changeset
        end
      end)

    # Split valid/invalid changesets after before_actions processing
    # Invalid changesets become {:error, error, changeset} tuples
    # after_transaction hooks for these will run later in process_results
    {batch, invalid_changeset_errors} =
      Ash.Actions.Helpers.Bulk.split_valid_invalid_changesets(batch, opts)

    # Build index maps from valid changesets only
    {changesets_by_ref, changesets_by_index} = index_changesets(batch, context_key)

    case batch do
      [] ->
        invalid_changeset_errors

      batch ->
        batch
        |> Enum.group_by(&{&1.atomics, &1.filter})
        |> Enum.flat_map(fn {_atomics, batch} ->
          result =
            case action.manual do
              {mod, manual_opts} ->
                source_context =
                  case batch do
                    [cs | _] -> cs.context
                    [] -> %{}
                  end

                if function_exported?(mod, :bulk_update, 3) do
                  mod.bulk_update(batch, manual_opts, %Ash.Resource.ManualUpdate.BulkContext{
                    actor: opts[:actor],
                    select: opts[:select],
                    source_context: source_context,
                    batch_size: opts[:batch_size],
                    authorize?: opts[:authorize?],
                    tracer: opts[:tracer],
                    domain: domain,
                    return_records?:
                      opts[:return_records?] || must_return_records? ||
                        must_return_records_for_changes?,
                    return_notifications?: opts[:return_notifications?] || false,
                    return_errors?: opts[:return_errors?] || false,
                    tenant: Ash.ToTenant.to_tenant(opts[:tenant], resource)
                  })
                else
                  ctx = %Ash.Resource.ManualUpdate.Context{
                    actor: opts[:actor],
                    source_context: source_context,
                    select: opts[:select],
                    authorize?: opts[:authorize?],
                    tracer: opts[:tracer],
                    domain: domain,
                    return_notifications?: opts[:return_notifications?] || false,
                    tenant: opts[:tenant]
                  }

                  [changeset] = batch

                  [
                    mod.update(changeset, manual_opts, ctx)
                    |> Ash.Actions.BulkManualActionHelpers.process_non_bulk_result(
                      changeset,
                      :bulk_update,
                      &Ash.Actions.Helpers.Bulk.store_notification/3,
                      ref,
                      opts
                    )
                  ]
                end
                |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                |> Ash.Actions.BulkManualActionHelpers.process_bulk_results(
                  mod,
                  :bulk_update,
                  &Ash.Actions.Helpers.Bulk.store_notification/3,
                  ref,
                  opts,
                  batch,
                  changesets_by_ref,
                  changesets_by_index
                )

              _ ->
                Enum.reduce_while(
                  batch,
                  {:ok, []},
                  fn changeset, {:ok, results} ->
                    changed? =
                      Ash.Changeset.changing_attributes?(changeset) or
                        not Enum.empty?(changeset.atomics)

                    if changed? do
                      changeset =
                        if Enum.empty?(changeset.attributes) &&
                             Ash.DataLayer.data_layer_can?(changeset.resource, :atomic_update) do
                          Ash.Changeset.atomic_defaults(changeset)
                        else
                          Ash.Changeset.set_defaults(changeset, :update, true)
                        end
                        |> Ash.Changeset.set_action_select()

                      resource
                      |> Ash.DataLayer.update(changeset)
                      |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                      |> case do
                        {:ok, result} ->
                          result =
                            result
                            |> Ash.Resource.put_metadata(
                              metadata_key,
                              changeset.context[context_key].index
                            )
                            |> Ash.Resource.put_metadata(
                              ref_metadata_key,
                              changeset.context[context_key].ref
                            )

                          {:cont, {:ok, [result | results]}}

                        {:error, %Ash.Error.Changes.StaleRecord{}} ->
                          {:cont, {:ok, results}}

                        {:error, error} ->
                          {:halt, {:error, error}}
                      end
                    else
                      result =
                        changeset.data
                        |> Ash.Resource.put_metadata(
                          metadata_key,
                          changeset.context[context_key].index
                        )
                        |> Ash.Resource.put_metadata(
                          ref_metadata_key,
                          changeset.context[context_key].ref
                        )

                      {:cont, {:ok, [result | results]}}
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
            # Manual action path: already tagged with changesets
            # Don't throw on error here - let errors flow through to process_results
            # so that after_transaction hooks can run and potentially convert errors to success
            {:manual_tagged, tagged_results} ->
              tagged_results

            {:ok, result} ->
              result

            {:error, error} ->
              error
              |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
              |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
              |> Ash.Helpers.error()
              |> List.wrap()
          end
        end)
        # Wrap results in tuples with embedded changesets
        |> Enum.flat_map(fn
          # Already tagged (from manual path) - pass through
          {:ok, result, changeset} when is_struct(changeset, Ash.Changeset) ->
            [{:ok, result, changeset}]

          {:error, error, changeset} when is_struct(changeset, Ash.Changeset) ->
            [{:error, error, changeset}]

          # Data layer path - needs changeset lookup
          result when is_struct(result) ->
            changeset =
              Ash.Actions.Helpers.Bulk.lookup_changeset(
                result,
                changesets_by_ref,
                changesets_by_index,
                index_key: metadata_key,
                ref_key: ref_metadata_key
              )

            [{:ok, result, changeset}]

          {:error, error} ->
            # Attach error to all changesets in the batch
            batch |> Enum.map(&{:error, error, &1})
        end)
        |> Enum.concat(invalid_changeset_errors)
    end
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

  @spec run_after_action_hooks(
          batch_results :: [Ash.Actions.Helpers.Bulk.tagged_result()],
          opts :: keyword(),
          domain :: Ash.Domain.t(),
          ref :: reference()
        ) :: [Ash.Actions.Helpers.Bulk.tagged_result()]
  defp run_after_action_hooks(batch_results, opts, domain, ref) do
    Enum.flat_map(batch_results, fn
      {:ok, result, changeset} ->
        case manage_relationships(result, domain, changeset,
               actor: opts[:actor],
               tenant: opts[:tenant],
               authorize?: opts[:authorize?]
             ) do
          {:ok, result, %{notifications: new_notifications, new_changeset: changeset}} ->
            Ash.Actions.Helpers.Bulk.store_notification(ref, new_notifications, opts)

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

                [{:error, error, changeset}]

              {:ok, result_after_action, changeset, %{notifications: more_new_notifications}} ->
                Ash.Actions.Helpers.Bulk.store_notification(ref, more_new_notifications, opts)

                result =
                  Map.put(
                    result_after_action,
                    :__metadata__,
                    Map.merge(result_after_action.__metadata__, result.__metadata__)
                  )

                [{:ok, result, changeset}]
            end

          {:error, error} ->
            [{:error, error, changeset}]
        end

      # Pass through error tuples unchanged
      other ->
        [other]
    end)
  end

  @spec process_results(
          tagged_results :: [Ash.Actions.Helpers.Bulk.tagged_result_with_hooks()],
          opts :: keyword(),
          ref :: reference(),
          context_key :: atom(),
          metadata_key :: atom(),
          resource :: Ash.Resource.t(),
          domain :: Ash.Domain.t(),
          base_changeset :: Ash.Changeset.t()
        ) :: [Ash.Resource.record() | {:error, term()}]
  defp process_results(
         tagged_results,
         opts,
         ref,
         context_key,
         metadata_key,
         resource,
         domain,
         base_changeset
       ) do
    need_notifications? = Ash.Actions.Helpers.Bulk.need_notifications?(opts)

    {results, changeset_by_id} =
      Enum.flat_map_reduce(tagged_results, %{}, fn
        {:ok, result, changeset}, changeset_map ->
          try do
            case Ash.Changeset.run_after_transactions({:ok, result}, changeset) do
              {:ok, result} ->
                Process.put({:any_success?, ref}, true)

                if opts[:return_records?] || need_notifications? do
                  changeset_id = make_ref()

                  metadata =
                    if index = changeset.context[context_key][:index] do
                      %{metadata_key => index, :bulk_changeset_id => changeset_id}
                    else
                      %{bulk_changeset_id: changeset_id}
                    end

                  {[Ash.Resource.set_metadata(result, metadata)],
                   Map.put(changeset_map, changeset_id, changeset)}
                else
                  {[], changeset_map}
                end

              {:error, error} ->
                error_result =
                  error
                  |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                  |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                  |> Ash.Helpers.error()
                  |> List.wrap()

                {error_result, changeset_map}
            end
          rescue
            e ->
              error_result =
                e
                |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                |> Ash.Helpers.error()
                |> List.wrap()

              {error_result, changeset_map}
          end

        {:ok_hooks_done, result, changeset}, changeset_map ->
          Process.put({:any_success?, ref}, true)

          if opts[:return_records?] || need_notifications? do
            changeset_id = make_ref()

            metadata =
              if index = changeset.context[context_key][:index] do
                %{metadata_key => index, :bulk_changeset_id => changeset_id}
              else
                %{bulk_changeset_id: changeset_id}
              end

            {[Ash.Resource.set_metadata(result, metadata)],
             Map.put(changeset_map, changeset_id, changeset)}
          else
            {[], changeset_map}
          end

        {:error, error, changeset}, changeset_map ->
          try do
            case Ash.Changeset.run_after_transactions({:error, error}, changeset) do
              {:ok, result} ->
                Process.put({:any_success?, ref}, true)

                if opts[:return_records?] || need_notifications? do
                  changeset_id = make_ref()

                  metadata =
                    if index = changeset.context[context_key][:index] do
                      %{metadata_key => index, :bulk_changeset_id => changeset_id}
                    else
                      %{bulk_changeset_id: changeset_id}
                    end

                  {[Ash.Resource.set_metadata(result, metadata)],
                   Map.put(changeset_map, changeset_id, changeset)}
                else
                  {[], changeset_map}
                end

              {:error, error} ->
                error_result =
                  error
                  |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                  |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                  |> Ash.Helpers.error()
                  |> List.wrap()

                {error_result, changeset_map}
            end
          rescue
            e ->
              error_result =
                e
                |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                |> Ash.Helpers.error()
                |> List.wrap()

              {error_result, changeset_map}
          end

        {:error_hooks_done, error, _changeset}, changeset_map ->
          {[{:error, error}], changeset_map}
      end)

    # Separate records from error tuples before loading
    {records, errors} =
      Enum.split_with(results, fn
        {:error, _} -> false
        _ -> true
      end)

    loaded_records =
      records
      |> load_data(domain, resource, base_changeset, opts)
      |> case do
        {:ok, records} ->
          Enum.reject(records, & &1.__metadata__[:private][:missing_from_data_layer])

        {:error, error} ->
          error
          |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
          |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
          |> Ash.Helpers.error()
          |> List.wrap()
      end

    Ash.Actions.Helpers.Bulk.store_notifications_for_loaded_records(
      loaded_records,
      changeset_by_id,
      ref,
      &notification/3,
      opts
    )

    cleaned_records = Ash.Actions.Helpers.Bulk.clean_changeset_id_metadata(loaded_records)

    if opts[:return_records?] do
      cleaned_records ++ errors
    else
      errors
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
           tenant: opts[:tenant],
           action:
             Ash.Resource.Info.primary_action(changeset.resource, :read) || changeset.action,
           actor: opts[:actor],
           authorize?: opts[:authorize?],
           tracer: opts[:tracer]
         ) do
      {:ok, records} ->
        Ash.load(
          records,
          List.wrap(changeset.load),
          reuse_values?: true,
          tenant: opts[:tenant],
          action: Ash.Resource.Info.primary_action(changeset.resource, :read) || changeset.action,
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

  @doc """
  Runs bulk after_batch changes on a list of tagged results.

  Called within transaction to execute batch change callbacks.
  """
  @spec run_bulk_after_changes(
          changes :: map(),
          all_changes :: [{map(), non_neg_integer()}],
          results :: [Ash.Actions.Helpers.Bulk.tagged_result()],
          changesets :: [Ash.Changeset.t()],
          opts :: keyword(),
          ref :: reference(),
          resource :: Ash.Resource.t(),
          metadata_key :: atom()
        ) :: [Ash.Actions.Helpers.Bulk.tagged_result()]
  def run_bulk_after_changes(
        changes,
        all_changes,
        results,
        changesets,
        opts,
        ref,
        resource,
        metadata_key
      ) do
    source_context =
      case changesets do
        [cs | _] -> cs.context
        _ -> %{}
      end

    {results, errors} =
      results
      |> Enum.split_with(fn
        {:ok, _, _} ->
          true

        _other ->
          false
      end)

    context =
      struct(
        Ash.Resource.Change.Context,
        %{
          bulk?: true,
          actor: opts[:actor],
          source_context: source_context,
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        }
      )

    all_changes
    |> Enum.reduce(results, fn
      {%{validation: _}, _index}, results ->
        results

      {%{change: {module, change_opts}}, index}, results ->
        {results, errors} =
          results
          |> Enum.split_with(fn
            {:ok, _, _} ->
              true

            _other ->
              false
          end)

        if changes[index] == :all do
          # Transform {:ok, result, changeset} to {changeset, result} for after_batch callbacks
          results_for_callback =
            Enum.map(results, fn {:ok, result, changeset} -> {changeset, result} end)

          case change_opts do
            {:templated, change_opts} ->
              if module.has_after_batch?() &&
                   module.has_batch_change?() &&
                   module.batch_callbacks?(changesets, change_opts, context) do
                module.after_batch(
                  results_for_callback,
                  change_opts,
                  context
                )
                |> case do
                  :ok ->
                    results

                  callback_results ->
                    callback_results
                    |> Enum.zip(results)
                    |> Enum.map(fn
                      {{:ok, result}, {:ok, _old_result, changeset}} ->
                        {:ok, result, changeset}

                      {{:error, error}, {:ok, _old_result, changeset}} ->
                        {:error, error, changeset}

                      {notification, _} ->
                        notification
                    end)
                end
              else
                results
              end

            change_opts ->
              Enum.flat_map(results_for_callback, fn {changeset, record} ->
                change_opts =
                  templated_opts(
                    change_opts,
                    opts[:actor],
                    changeset.to_tenant,
                    changeset.arguments,
                    changeset.context,
                    changeset
                  )

                if module.has_after_batch?() &&
                     module.has_batch_change?() &&
                     module.batch_callbacks?(changesets, change_opts, context) do
                  module.after_batch(
                    [{changeset, record}],
                    change_opts,
                    context
                  )
                  |> case do
                    :ok ->
                      [{:ok, record, changeset}]

                    results ->
                      results
                      |> Enum.zip(changesets)
                      |> Enum.map(fn
                        {{:ok, result}, changeset} ->
                          {:ok, result, changeset}

                        {{:error, error}, changeset} ->
                          {:error, error, changeset}

                        {notification, _changeset} ->
                          notification
                      end)
                  end
                else
                  [{:ok, record, changeset}]
                end
              end)
          end
          |> handle_after_batch_results(results, ref, resource, opts)
        else
          {matches, non_matches} =
            results
            |> Enum.split_with(fn
              {:ok, result, _changeset} ->
                result.__metadata__[metadata_key] in List.wrap(changes[index])
            end)

          # Transform {:ok, result, changeset} to {changeset, result} for after_batch callbacks
          matches_for_callback =
            Enum.map(matches, fn {:ok, result, changeset} -> {changeset, result} end)

          after_batch_results =
            case change_opts do
              {:templated, change_opts} ->
                if module.has_after_batch?() &&
                     module.has_batch_change?() &&
                     module.batch_callbacks?(changesets, change_opts, context) do
                  module.after_batch(
                    matches_for_callback,
                    change_opts,
                    struct(
                      Ash.Resource.Change.Context,
                      %{
                        bulk?: true,
                        source_context: source_context,
                        actor: opts[:actor],
                        tenant: opts[:tenant],
                        tracer: opts[:tracer],
                        authorize?: opts[:authorize?]
                      }
                    )
                  )
                  |> case do
                    :ok ->
                      matches

                    callback_results ->
                      callback_results
                      |> Enum.zip(matches)
                      |> Enum.map(fn
                        {{:ok, result}, {:ok, _old_result, changeset}} ->
                          {:ok, result, changeset}

                        {{:error, error}, {:ok, _old_result, changeset}} ->
                          {:error, error, changeset}

                        {notification, _} ->
                          notification
                      end)
                  end
                else
                  matches
                end

              change_opts ->
                Enum.flat_map(matches_for_callback, fn {changeset, record} = result ->
                  if module.has_after_batch?() &&
                       module.has_batch_change?() &&
                       module.batch_callbacks?(changesets, change_opts, context) do
                    change_opts =
                      templated_opts(
                        change_opts,
                        opts[:actor],
                        changeset.to_tenant,
                        changeset.arguments,
                        changeset.context,
                        changeset
                      )

                    module.after_batch(
                      [result],
                      change_opts,
                      struct(
                        Ash.Resource.Change.Context,
                        %{
                          bulk?: true,
                          source_context: source_context,
                          actor: opts[:actor],
                          tenant: opts[:tenant],
                          tracer: opts[:tracer],
                          authorize?: opts[:authorize?]
                        }
                      )
                    )
                    |> case do
                      :ok ->
                        [{:ok, record, changeset}]

                      results ->
                        results
                        |> Enum.zip(changesets)
                        |> Enum.map(fn
                          {{:ok, result}, changeset} ->
                            {:ok, result, changeset}

                          {{:error, error}, changeset} ->
                            {:error, error, changeset}

                          {notification, _changeset} ->
                            notification
                        end)
                    end
                  else
                    [{:ok, record, changeset}]
                  end
                end)
            end
            |> handle_after_batch_results(matches, ref, resource, opts)

          Enum.concat([after_batch_results, non_matches])
        end
        |> Enum.concat(errors)
    end)
    |> Enum.concat(errors)
  end

  defp handle_after_batch_results(results, _matches, ref, resource, opts) do
    results
    |> Enum.flat_map(fn
      {%Ash.Notifier.Notification{} = notification, _} ->
        Ash.Actions.Helpers.Bulk.store_notification(ref, notification, opts)

      {:ok, result, changeset} ->
        [{:ok, result, changeset}]

      {:error, error, changeset} ->
        if opts[:transaction] && opts[:rollback_on_error?] do
          if Ash.DataLayer.in_transaction?(resource) do
            Ash.DataLayer.rollback(
              resource,
              error
            )
          end
        end

        if opts[:stop_on_error?] && !opts[:return_stream?] do
          throw({:error, Ash.Error.to_error_class(error)})
        end

        [{:error, error, changeset}]
    end)
  end

  defp notification(changeset, result, opts) do
    Ash.Actions.Helpers.resource_notification(changeset, result, opts)
  end

  def run_action_changes(
        batch,
        all_changes,
        action,
        resource,
        actor,
        authorize?,
        tracer,
        tenant,
        context_key
      ) do
    source_context =
      case batch do
        [cs | _] ->
          cs.context

        _ ->
          %{}
      end

    context = %{
      actor: actor,
      source_context: source_context,
      authorize?: authorize? || false,
      tracer: tracer,
      tenant: tenant
    }

    Enum.reduce(
      all_changes,
      %{must_return_records?: false, re_sort?: false, batch: batch, changes: %{}},
      fn
        {%{validation: {module, _opts}} = validation, _change_index}, %{batch: batch} = state ->
          validation_context =
            struct(
              Ash.Resource.Validation.Context,
              Map.put(context, :message, validation.message)
            )

          batch =
            if module.has_validate?() &&
                 Enum.all?(validation.where, fn {module, _opts} ->
                   module.has_validate?()
                 end) do
              validate_batch_non_atomically(validation, batch, validation_context, actor)
            else
              if module.atomic?() do
                validate_batch_atomically(validation, batch, validation_context, context, actor)
              else
                raise """
                Cannot use a non-atomic validation with an atomic condition.

                Attempting to run action: `#{inspect(resource)}.#{action.name}`
                """
              end
            end

          %{
            state
            | must_return_records?: state.must_return_records?,
              batch: batch,
              changes: state.changes
          }

        {%{change: {module, change_opts}} = change, change_index}, %{batch: batch} = state ->
          {matches, non_matches} =
            if Enum.empty?(change.where) && !change.only_when_valid? do
              {batch, []}
            else
              Enum.split_with(batch, fn changeset ->
                Enum.all?(change.where || [], fn {module, opts} ->
                  if module.has_validate?() do
                    opts =
                      Ash.Actions.Helpers.templated_opts(
                        opts,
                        actor,
                        changeset.tenant,
                        changeset.arguments,
                        changeset.context,
                        changeset
                      )

                    {:ok, opts} = module.init(opts)

                    if change.only_when_valid? do
                      changeset.valid? &&
                        Ash.Resource.Validation.validate(
                          module,
                          changeset,
                          opts,
                          struct(Ash.Resource.Validation.Context, context)
                        ) == :ok
                    else
                      Ash.Resource.Validation.validate(
                        module,
                        changeset,
                        opts,
                        struct(Ash.Resource.Validation.Context, context)
                      ) == :ok
                    end
                  else
                    true
                  end
                end)
              end)
            end

          if Enum.empty?(matches) do
            %{
              state
              | must_return_records?: state.must_return_records?,
                batch: Enum.concat(matches, non_matches),
                changes: state.changes
            }
          else
            matches = batch_change(change, matches, context, actor)

            must_return_records? =
              state.must_return_records? ||
                Enum.any?(batch, fn item ->
                  item.relationships not in [nil, %{}] || !Enum.empty?(item.after_action)
                end) ||
                (module.has_after_batch?() &&
                   module.has_batch_change?() &&
                   module.batch_callbacks?(batch, change_opts, context))

            match_indices =
              if Enum.empty?(non_matches) do
                :all
              else
                Enum.map(matches, & &1.context[context_key].index)
              end

            %{
              state
              | must_return_records?: must_return_records?,
                batch: Enum.concat(matches, non_matches),
                re_sort?: true,
                changes:
                  Map.put(
                    state.changes,
                    change_index,
                    match_indices
                  )
            }
          end
      end
    )
  end

  defp validate_batch_non_atomically(
         %{validation: {module, opts}} = validation,
         batch,
         validation_context,
         actor
       ) do
    Enum.map(batch, fn changeset ->
      if Enum.all?(validation.where || [], fn {module, opts} ->
           opts =
             Ash.Actions.Helpers.templated_opts(
               opts,
               actor,
               changeset.to_tenant,
               changeset.arguments,
               changeset.context,
               changeset
             )

           {:ok, opts} = module.init(opts)

           Ash.Resource.Validation.validate(
             module,
             changeset,
             opts,
             validation_context
           ) == :ok
         end) do
        opts =
          Ash.Actions.Helpers.templated_opts(
            opts,
            actor,
            changeset.to_tenant,
            changeset.arguments,
            changeset.context,
            changeset
          )

        {:ok, opts} = module.init(opts)

        case Ash.Resource.Validation.validate(
               module,
               changeset,
               opts,
               validation_context
             ) do
          :ok ->
            changeset

          {:error, error} ->
            if validation.message do
              error = Ash.Error.override_validation_message(error, validation.message)
              Ash.Changeset.add_error(changeset, error)
            else
              Ash.Changeset.add_error(changeset, error)
            end
        end
      else
        changeset
      end
    end)
  end

  defp validate_batch_atomically(validation, batch, validation_context, context, actor) do
    Enum.map(batch, fn changeset ->
      validation = %{
        validation
        | opts:
            templated_opts(
              validation.opts,
              actor,
              changeset.tenant,
              changeset.arguments,
              changeset.context,
              changeset
            )
      }

      case Ash.Changeset.split_atomic_conditions(
             validation,
             changeset,
             actor,
             validation_context
           ) do
        {:ok, validation_with_remaining_conditions} ->
          Ash.Changeset.run_atomic_validation(
            changeset,
            validation_with_remaining_conditions,
            context
          )

        :skip ->
          changeset
      end
    end)
  end

  def batch_change(%{change: {module, change_opts}, where: where} = change, batch, context, actor) do
    must_be_atomic? =
      Enum.any?(change.where, fn {mod, _} ->
        !mod.has_validate?()
      end)

    case change_opts do
      {:templated, change_opts} ->
        cond do
          !must_be_atomic? && module.has_batch_change?() &&
              module.batch_callbacks?(batch, change_opts, context) ->
            {:ok, change_opts} = module.init(change_opts)
            module.batch_change(batch, change_opts, context)

          !must_be_atomic? && module.has_change?() ->
            Enum.map(batch, fn changeset ->
              {:ok, change_opts} = module.init(change_opts)

              Ash.Resource.Change.change(
                module,
                changeset,
                change_opts,
                struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
              )
            end)

          module.atomic?() ->
            Enum.map(batch, fn changeset ->
              change = %{
                change
                | where:
                    Enum.filter(where, fn {module, _opts} ->
                      module.atomic?()
                    end)
                    |> Enum.map(fn {module, change_opts} ->
                      {module,
                       templated_opts(
                         change_opts,
                         actor,
                         changeset.to_tenant,
                         changeset.arguments,
                         changeset.context,
                         changeset
                       )}
                    end),
                  change:
                    {module,
                     templated_opts(
                       change_opts,
                       actor,
                       changeset.to_tenant,
                       changeset.arguments,
                       changeset.context,
                       changeset
                     )}
              }

              case Ash.Changeset.run_atomic_change(changeset, change, context) do
                {:not_atomic, reason} ->
                  Ash.Changeset.add_error(
                    changeset,
                    "Could not be made atomic: #{inspect(reason)}"
                  )

                changeset ->
                  changeset
              end
            end)

          true ->
            if must_be_atomic? do
              raise "#{inspect(module)} cannot be paired with atomic conditions, as it does not implement `atomic/3`"
            else
              raise "#{inspect(module)} must define at least one of `atomic/3`, `change/3` or `batch_change/3`"
            end
        end

      change_opts ->
        cond do
          !must_be_atomic? && module.has_batch_change?() ->
            Enum.flat_map(batch, fn changeset ->
              if module.batch_callbacks?(batch, change_opts, context) do
                change_opts =
                  templated_opts(
                    change_opts,
                    actor,
                    changeset.to_tenant,
                    changeset.arguments,
                    changeset.context,
                    changeset
                  )

                {:ok, change_opts} = module.init(change_opts)
                module.batch_change(batch, change_opts, context)
              else
                [
                  Ash.Resource.Change.change(
                    module,
                    changeset,
                    change_opts,
                    struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
                  )
                ]
              end
            end)

          !must_be_atomic? && module.has_change?() ->
            Enum.map(batch, fn changeset ->
              change_opts =
                templated_opts(
                  change_opts,
                  actor,
                  changeset.to_tenant,
                  changeset.arguments,
                  changeset.context,
                  changeset
                )

              {:ok, change_opts} = module.init(change_opts)

              Ash.Resource.Change.change(
                module,
                changeset,
                change_opts,
                struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
              )
            end)

          module.atomic?() ->
            Enum.map(batch, fn changeset ->
              change = %{
                change
                | where:
                    Enum.filter(where, fn {module, _opts} ->
                      module.atomic?()
                    end),
                  change:
                    {module,
                     templated_opts(
                       change_opts,
                       actor,
                       changeset.to_tenant,
                       changeset.arguments,
                       changeset.context,
                       changeset
                     )}
              }

              case Ash.Changeset.run_atomic_change(changeset, change, context) do
                {:not_atomic, reason} ->
                  Ash.Changeset.add_error(
                    changeset,
                    "Could not be made atomic: #{inspect(reason)}"
                  )

                changeset ->
                  changeset
              end
            end)

          true ->
            if must_be_atomic? do
              raise "#{inspect(module)} cannot be paired with atomic conditions, as it does not implement `atomic/3`"
            else
              raise "#{inspect(module)} must define at least one of `atomic/3`, `change/3` or `batch_change/3`"
            end
        end
    end
  end

  defp templated_opts({:templated, opts}, _actor, _tenant, _arguments, _context, _changeset),
    do: opts

  defp templated_opts(opts, actor, tenant, arguments, context, changeset) do
    Ash.Expr.fill_template(
      opts,
      actor: actor,
      tenant: tenant,
      args: arguments,
      context: context,
      changeset: changeset
    )
  end

  @doc false
  def get_read_action(resource, action, opts) do
    case opts[:read_action] do
      nil ->
        case action do
          %{atomic_upgrade_with: read_action} when not is_nil(read_action) ->
            Ash.Resource.Info.action(resource, read_action) ||
              raise "No such read action in #{inspect(resource)}.#{action.name}.atomic_upgrade_with: #{read_action}"

          action when is_atom(action) and not is_nil(action) ->
            action =
              Ash.Resource.Info.action(resource, action) ||
                raise "No such update action #{inspect(resource)}.#{action}"

            if read_action = action.atomic_upgrade_with do
              Ash.Resource.Info.action(resource, read_action) ||
                raise "No such read action in #{inspect(resource)}.#{action.name}.atomic_upgrade_with: #{read_action}"
            else
              Ash.Resource.Info.primary_action!(resource, :read)
            end

          _ ->
            Ash.Resource.Info.primary_action!(resource, :read)
        end

      action ->
        Ash.Resource.Info.action(resource, action)
    end
  end

  defp clear_ref_metadata(record) do
    Ash.Resource.set_metadata(record, Map.delete(record.__metadata__, :bulk_action_ref))
  end

  defp clear_ref_metadata_from_records(%{records: records} = result) when is_list(records) do
    %{result | records: Enum.map(records, &clear_ref_metadata/1)}
  end

  defp clear_ref_metadata_from_records(result), do: result
end
