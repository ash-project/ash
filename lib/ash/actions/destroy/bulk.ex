# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Destroy.Bulk do
  @moduledoc false
  require Ash.Query

  @spec run(Ash.Domain.t(), Enumerable.t() | Ash.Query.t(), atom(), input :: map, Keyword.t()) ::
          Ash.BulkResult.t()

  def run(domain, resource, action, input, opts, not_atomic_reason \\ nil)

  def run(domain, stream, action, input, opts, not_atomic_reason)
      when is_atom(action) and not is_nil(action) do
    resource = opts[:resource]

    opts =
      if opts[:return_notifications?] do
        Keyword.put(opts, :notify?, true)
      else
        opts
      end

    case Ash.Helpers.get_action(resource, opts, :destroy, action) do
      {:ok, action} ->
        run(
          domain,
          stream,
          action,
          input,
          opts,
          not_atomic_reason
        )

      {:error, error} ->
        %Ash.BulkResult{status: :error, errors: [Ash.Error.to_error_class(error)]}
    end
  end

  def run(domain, stream, nil, input, opts, not_atomic_reason) do
    resource = opts[:resource]

    run(
      domain,
      stream,
      Ash.Resource.Info.primary_action!(resource, :destroy),
      input,
      opts,
      not_atomic_reason
    )
  end

  def run(domain, stream, %{soft?: true} = action, input, opts, not_atomic_reason) do
    Ash.Actions.Update.Bulk.run(domain, stream, action, input, opts, not_atomic_reason)
  end

  def run(domain, resource, action, input, opts, not_atomic_reason) when is_atom(resource) do
    run(domain, Ash.Query.new(resource), action, input, opts, not_atomic_reason)
  end

  def run(domain, %Ash.Query{} = query, action, input, opts, not_atomic_reason) do
    opts = set_strategy(opts, query.resource)

    opts = select(opts, query.resource)

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
            Ash.Resource.Info.primary_action!(query.resource, :read).name,
            %{},
            actor: opts[:actor],
            tenant: opts[:tenant],
            context: %{query_for: :bulk_destroy}
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
          {:not_atomic, "Manual read actions cannot be destroyed atomically"}

        !Enum.empty?(query.before_action) ->
          {:not_atomic, "cannot atomically update a query if it has `before_action` hooks"}

        !Enum.empty?(query.after_action) ->
          {:not_atomic, "cannot atomically update a query if it has `after_action` hooks"}

        changeset = opts[:atomic_changeset] ->
          changeset

        Ash.DataLayer.data_layer_can?(query.resource, :destroy_query) ->
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
          {:not_atomic, "data layer does not support destroying a query"}
      end

    case fully_atomic_changeset do
      {:not_atomic, reason} ->
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
          |> Keyword.put(:domain, domain)
          |> Keyword.delete(:load)

        query =
          Ash.Query.do_filter(query, opts[:filter])

        if query.limit && query.limit < (opts[:batch_size] || 100) do
          read_opts = Keyword.take(read_opts, Keyword.keys(Ash.read_opts()))

          case Ash.Actions.Read.unpaginated_read(query, query.action, read_opts) do
            {:ok, results} ->
              run(
                domain,
                results,
                action,
                input,
                Keyword.merge(opts,
                  resource: query.resource,
                  input_was_stream?: false
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
                      action: query.action.name,
                      requested_strategies: opts[:strategy],
                      not_stream_reason: "could not stream the query",
                      footer: "Non stream reason:\n\n" <> Exception.message(exception)
                    )
                  )
                ]
              }

            _ ->
              read_opts = Keyword.take(read_opts, Ash.stream_opt_keys())

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
                Keyword.merge(opts,
                  resource: query.resource,
                  input_was_stream?: false
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

        atomic_changeset =
          if opts[:context] do
            Ash.Changeset.set_context(atomic_changeset, opts[:context])
          else
            atomic_changeset
          end

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
              action.changes ++ Ash.Resource.Info.changes(atomic_changeset.resource, :destroy),
              fn
                %{change: {module, change_opts}} ->
                  module.has_after_batch?() &&
                    module.batch_callbacks?(query, change_opts, context)

                _ ->
                  false
              end
            )

          result =
            if (has_after_batch_hooks? || !Enum.empty?(atomic_changeset.after_action)) &&
                 Keyword.get(opts, :transaction, true) do
              Ash.DataLayer.transaction(
                List.wrap(atomic_changeset.resource) ++ action.touches_resources,
                fn ->
                  do_atomic_destroy(
                    query,
                    atomic_changeset,
                    has_after_batch_hooks?,
                    input,
                    opts
                  )
                end,
                opts[:timeout],
                %{
                  type: :bulk_destroy,
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
               do_atomic_destroy(
                 query,
                 atomic_changeset,
                 has_after_batch_hooks?,
                 input,
                 opts
               )}
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

              # For atomic, disable stop_on_error? since there are no batches to stop between
              atomic_opts = Keyword.put(opts, :stop_on_error?, false)

              processed =
                tagged_results
                |> process_results(
                  atomic_opts,
                  ref,
                  atomic_changeset.domain,
                  atomic_changeset.resource,
                  atomic_changeset
                )
                |> Enum.to_list()

              any_success? = Process.get({:any_success?, ref}, false)

              # Merge notifications from do_atomic_destroy with those from process_results
              all_notifications =
                notifications ++ (Process.delete({:bulk_notifications, ref}) || [])

              Ash.Actions.BulkManualActionHelpers.build_bulk_result(
                processed,
                any_success?,
                all_notifications,
                opts
              )
          end
          |> handle_atomic_notifications(atomic_changeset.resource, action, notify?, opts)
        after
          if notify? do
            Process.delete(:ash_started_transaction?)
          end
        end
    end
  end

  def run(domain, stream, action, input, opts, not_atomic_reason) do
    resource = opts[:resource]

    opts = select(opts, resource)

    opts =
      if opts[:return_notifications?] do
        Keyword.put(opts, :notify?, true)
      else
        opts
      end

    opts = set_strategy(opts, resource, Keyword.get(opts, :input_was_stream?, true))

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

    if opts[:transaction] == :all &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      notify? = opts[:notify?] && !Process.put(:ash_started_transaction?, true)

      try do
        Ash.DataLayer.transaction(
          List.wrap(resource) ++ action.touches_resources,
          fn ->
            do_run(
              domain,
              stream,
              action,
              input,
              Keyword.merge(opts, notify?: opts[:notify?], return_notifications?: opts[:notify?]),
              not_atomic_reason
            )
          end,
          opts[:timeout],
          %{
            type: :bulk_destroy,
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

            handle_bulk_result(bulk_result, resource, action, opts)

          {:error, error} ->
            handle_bulk_result(
              %Ash.BulkResult{errors: [error], status: :error},
              resource,
              action,
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
      |> do_run(stream, action, input, opts, not_atomic_reason)
      |> handle_bulk_result(resource, action, opts)
    end
  end

  defp select(opts, resource) do
    if opts[:select] do
      opts
    else
      all_attributes =
        resource
        |> Ash.Resource.Info.public_attributes()
        |> Enum.map(& &1.name)

      Keyword.put(opts, :select, all_attributes)
    end
  end

  defp do_atomic_destroy(
         query,
         atomic_changeset,
         has_after_batch_hooks?,
         input,
         opts
       ) do
    atomic_changeset =
      if atomic_changeset.context[:data_layer][:use_atomic_destroy_data?] do
        atomic_changeset
      else
        %{
          atomic_changeset
          | data: %Ash.Changeset.OriginalDataNotAvailable{reason: :atomic_query_destroy}
        }
      end

    context =
      struct(Ash.Resource.Change.Context, %{
        bulk?: true,
        source_context: atomic_changeset.context,
        actor: opts[:actor],
        tenant: opts[:tenant],
        tracer: opts[:tracer],
        authorize?: opts[:authorize?]
      })

    {all_changes, conditional_after_batch_hooks, calculations} =
      Ash.Actions.Update.Bulk.hooks_and_calcs_for_update_query(
        atomic_changeset,
        context,
        query,
        opts
      )

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

    destroy_query_opts =
      opts
      |> Keyword.take([:tenant, :select])
      |> Map.new()
      |> Map.put(:return_records?, return_records?)
      |> Map.put(:calculations, calculations)
      |> Map.put(
        :action_select,
        action_select
      )

    with :ok <- validate_multitenancy(atomic_changeset.resource, atomic_changeset.action, opts),
         {:ok, query} <-
           authorize_bulk_query(query, atomic_changeset, opts),
         {:ok, atomic_changeset, query} <-
           authorize_atomic_changeset(query, atomic_changeset, opts),
         {query, atomic_changeset} <- add_changeset_filters(query, atomic_changeset),
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
         {:ok, query} <- Ash.Actions.Read.handle_multitenancy(query),
         {:ok, data_layer_query} <- Ash.Query.data_layer_query(query) do
      case Ash.DataLayer.destroy_query(
             data_layer_query,
             atomic_changeset,
             destroy_query_opts
           )
           |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, nil) do
        :ok ->
          # No records returned from data layer, just success
          {:ok, [], []}

        {:ok, results} ->
          results = List.wrap(results)

          {results, notifications} =
            if has_after_batch_hooks? do
              Ash.Actions.Update.Bulk.run_atomic_after_batch_hooks(
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
               "Returned from bulk query destroy: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
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
            Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
          else
            {:error,
             Ash.Error.to_error_class(error,
               bread_crumbs: [
                 "Returned from bulk query destroy: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
               ]
             )}
          end

        {:error, :no_rollback, error} ->
          {:error,
           Ash.Error.to_error_class(error,
             bread_crumbs: [
               "Returned from bulk query destroy: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
             ]
           )}

        {:error, error} ->
          if Ash.DataLayer.in_transaction?(atomic_changeset.resource) do
            Ash.DataLayer.rollback(atomic_changeset.resource, Ash.Error.to_error_class(error))
          else
            {:error,
             Ash.Error.to_error_class(error,
               bread_crumbs: [
                 "Returned from bulk query destroy: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
               ]
             )}
          end
      end
    else
      {:error, %Ash.Error.Forbidden.InitialDataRequired{}} ->
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
            # Fallback to stream strategy - returns complete BulkResult
            run(
              atomic_changeset.domain,
              query,
              atomic_changeset.action,
              input,
              Keyword.put(opts, :strategy, [:stream]),
              "authorization requires initial data"
            )
        end

      {:error, error} ->
        {:error,
         Ash.Error.to_error_class(error,
           bread_crumbs: [
             "Returned from bulk query destroy: #{inspect(atomic_changeset.resource)}.#{atomic_changeset.action.name}"
           ]
         )}
    end
  end

  defp set_strategy(opts, resource, inputs_is_enumerable? \\ false) do
    opts =
      if Ash.DataLayer.data_layer_can?(resource, :update_query) &&
           Ash.DataLayer.data_layer_can?(resource, :expr_error) do
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

  defp add_changeset_filters(query, changeset) do
    {Ash.Query.do_filter(query, changeset.filter), %{changeset | filter: nil}}
  end

  defp do_run(domain, stream, action, input, opts, not_atomic_reason) do
    resource = opts[:resource]
    opts = Ash.Actions.Helpers.set_opts(opts, domain)

    read_action = Ash.Actions.Update.Bulk.get_read_action(resource, action, opts)

    {context_cs, opts} =
      Ash.Actions.Helpers.set_context_and_get_opts(domain, Ash.Changeset.new(resource), opts)

    case validate_multitenancy(resource, action, opts) do
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
              {:not_atomic, "cannot atomically destroy a stream without a primary key"}

            !Ash.Resource.Info.primary_action(resource, :read) ->
              {:not_atomic, "cannot atomically destroy a stream without a primary read action"}

            read_action.manual ->
              {:not_atomic, "Manual read actions cannot be updated atomically"}

            Ash.DataLayer.data_layer_can?(resource, :destroy_query) ->
              opts =
                Keyword.update(
                  opts,
                  :context,
                  %{private: context_cs.context.private},
                  &Map.put(&1, :private, context_cs.context.private)
                )

              Ash.Changeset.fully_atomic_changeset(resource, action, input, opts)

            true ->
              {:not_atomic, "data layer does not support destroying a query"}
          end

        case fully_atomic_changeset do
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

          %Ash.Changeset{} = atomic_changeset ->
            query =
              resource
              |> Ash.Query.new()
              |> Map.put(:action, Ash.Resource.Info.primary_action!(resource, :read))

            case Ash.Actions.Read.Stream.stream_strategy(
                   query,
                   nil,
                   opts[:allow_stream_with] || :keyset
                 ) do
              {:error, exception} ->
                if :stream in opts[:strategy] do
                  do_stream_batches(domain, stream, action, input, opts)
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
              do_stream_batches(domain, stream, action, input, opts)
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

  defp validate_multitenancy(resource, action, opts) do
    if Ash.Resource.Info.multitenancy_strategy(resource) &&
         !Ash.Resource.Info.multitenancy_global?(resource) && !opts[:tenant] &&
         Map.get(action, :multitenancy) not in [:bypass, :bypass_all, :allow_global] &&
         get_in(opts, [:context, :shared, :private, :multitenancy]) not in [
           :bypass,
           :bypass_all,
           :allow_global
         ] do
      {:error, Ash.Error.Invalid.TenantRequired.exception(resource: resource)}
    else
      :ok
    end
  end

  defp do_atomic_batches(atomic_changeset, domain, stream, action, input, opts) do
    batch_size = opts[:batch_size] || 100
    resource = opts[:resource]
    ref = make_ref()
    pkey = Ash.Resource.Info.primary_key(resource)

    return_records? =
      opts[:notify?] || opts[:return_records?] ||
        !Enum.empty?(atomic_changeset.after_action) ||
        !Enum.empty?(atomic_changeset.after_transaction)

    stream
    |> Stream.chunk_every(batch_size)
    |> map_batches(
      resource,
      opts,
      ref,
      fn batch ->
        pkeys = [or: Enum.map(batch, &Map.take(&1, pkey))]

        read_action = Ash.Actions.Update.Bulk.get_read_action(resource, action, opts).name

        resource
        |> Ash.Query.for_read(read_action, %{},
          actor: opts[:actor],
          authorize?: false,
          context: Map.put(atomic_changeset.context, :query_for, :bulk_destroy),
          tenant: atomic_changeset.tenant,
          tracer: opts[:tracer]
        )
        |> Ash.Query.set_context(%{private: %{internal?: true}})
        |> Ash.Query.filter(^pkeys)
        |> Ash.Query.select([])
        |> then(fn query ->
          run(
            domain,
            query,
            action.name,
            input,
            actor: opts[:actor],
            authorize_query?: false,
            authorize?: opts[:authorize?],
            tenant: atomic_changeset.tenant,
            tracer: opts[:tracer],
            atomic_changeset: atomic_changeset,
            return_errors?: opts[:return_errors?],
            filter: opts[:filter],
            load: opts[:load],
            resource: opts[:resource],
            return_notifications?: opts[:return_notifications?],
            notify?: opts[:notify?],
            read_action: read_action,
            return_records?: return_records?,
            allow_stream_with: opts[:allow_stream_with],
            strategy: [:atomic]
          )
          |> case do
            %Ash.BulkResult{
              error_count: 0,
              records: records,
              notifications: notifications,
              status: status
            } ->
              Process.put({:any_success?, ref}, status != :error)

              store_notification(ref, notifications, opts)
              List.wrap(records)

            %Ash.BulkResult{
              errors: errors,
              notifications: notifications,
              error_count: error_count,
              status: status
            } ->
              Process.put({:any_success?, ref}, status != :error)
              store_notification(ref, notifications, opts)

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

                  # No error details but we have a count - create placeholder tuples
                  for _ <- 1..error_count, do: {:error, :transaction_error}

                true ->
                  []
              end
          end
        end)
      end
    )
    |> run_batches(ref, atomic_changeset.resource, atomic_changeset.action.name, opts)
  end

  defp do_stream_batches(domain, stream, action, input, opts) do
    resource = opts[:resource]

    manual_action_can_bulk? =
      case action.manual do
        {mod, _opts} ->
          function_exported?(mod, :bulk_destroy, 3)

        _ ->
          false
      end

    batch_size =
      if manual_action_can_bulk? do
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
          |> Stream.map(
            &setup_changeset(
              &1,
              action,
              opts,
              input,
              argument_names,
              domain
            )
          )
          |> handle_batch(domain, resource, action, all_changes, opts, ref, base_changeset)
        after
          if opts[:notify?] && !opts[:return_notifications?] do
            Ash.Notifier.notify(Process.delete({:bulk_notifications, ref}))
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
        # Collect all results (records and {:error, error} tuples)
        all_results = Enum.to_list(Stream.concat(changeset_stream))

        # Separate records from error tuples
        {records, error_tuples} =
          Enum.split_with(all_results, fn
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
                  "Returned from bulk destroy: #{inspect(resource)}.#{action_name}"
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

          result = %Ash.BulkResult{
            status: status,
            notifications: Process.delete({:bulk_notifications, ref})
          }

          {error_count, errors} = errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_notifications, ref})
      end
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
    |> Ash.Changeset.filter(opts[:filter])
    |> Map.put(:domain, domain)
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

  defp authorize_bulk_query(query, atomic_changeset, opts) do
    if opts[:authorize?] && opts[:authorize_query?] do
      case Ash.can(query, opts[:actor],
             return_forbidden_error?: true,
             pre_flight?: false,
             atomic_changeset: atomic_changeset,
             filter_with: opts[:authorize_query_with] || opts[:authorize_with] || :filter,
             run_queries?: false,
             maybe_is: false,
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

  defp authorize_atomic_changeset(query, changeset, opts) do
    if opts[:authorize?] do
      case Ash.can(changeset, opts[:actor],
             return_forbidden_error?: true,
             pre_flight?: false,
             maybe_is: false,
             atomic_changeset: changeset,
             no_check?: true,
             on_must_pass_strict_check:
               {:error, %Ash.Error.Forbidden.InitialDataRequired{source: changeset}},
             run_queries?: false,
             filter_with: opts[:authorize_changeset_with] || opts[:authorize_with] || :filter,
             alter_source?: true,
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

  defp pre_template_all_changes(action, resource, :destroy, base, actor, tenant) do
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
    Stream.resource(
      fn -> Process.delete({:bulk_notifications, ref}) end,
      fn
        [] ->
          {:halt, []}

        notifications ->
          {Stream.map(notifications || [], &{:notification, &1}), []}
      end,
      fn _ -> :ok end
    )
  end

  defp handle_batch(batch, domain, resource, action, all_changes, opts, ref, base_changeset) do
    %{
      must_return_records?: must_return_records_for_changes?,
      batch: batch,
      re_sort?: re_sort?,
      changes: changes
    } =
      Ash.Actions.Update.Bulk.run_action_changes(
        batch,
        all_changes,
        action,
        resource,
        opts[:actor],
        opts[:authorize?],
        opts[:tracer],
        opts[:tenant],
        :bulk_destroy
      )

    batch = authorize(batch, opts)

    batch =
      if re_sort? do
        Enum.sort_by(batch, & &1.context.bulk_destroy.index)
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
        :bulk_destroy,
        fn changeset ->
          case Ash.Actions.Destroy.run(
                 domain,
                 changeset,
                 action,
                 Keyword.put(opts, :return_destroyed?, opts[:return_records?])
               ) do
            :ok ->
              []

            {:ok, result} when not is_list(result) ->
              [
                Ash.Resource.set_metadata(result, %{
                  bulk_destroy_index: changeset.context.bulk_destroy.index,
                  bulk_action_ref: changeset.context.bulk_destroy.ref
                })
              ]

            {:ok, notifications} ->
              store_notification(ref, notifications, opts)

              []

            {:ok, result, notifications} ->
              store_notification(ref, notifications, opts)

              [
                Ash.Resource.set_metadata(result, %{
                  bulk_destroy_index: changeset.context.bulk_destroy.index,
                  bulk_action_ref: changeset.context.bulk_destroy.ref
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
                base_changeset,
                must_return_records_for_changes?,
                changes,
                must_be_simple_results
              )

            notifications = Process.get({:bulk_notifications, tmp_ref}) || []
            store_notification(ref, notifications, opts)

            result
          end,
          opts[:timeout],
          %{
            type: :bulk_destroy,
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
            # Invalid changesets already had hooks run, tagged as :ok_hooks_done/:error_hooks_done
            all_tagged_results = invalid_changeset_results ++ tagged_results

            process_results(
              all_tagged_results,
              opts,
              ref,
              domain,
              resource,
              base_changeset
            )
            |> Stream.concat(must_be_simple_results)
            |> then(fn stream ->
              if opts[:return_stream?] do
                stream
                |> Stream.map(fn
                  {:error, _} = error_tuple -> error_tuple
                  record -> {:ok, record}
                end)
                |> Stream.concat(notification_stream(ref))
              else
                stream
              end
            end)

          {:error, error} ->
            error
            |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
            |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
            |> Ash.Helpers.error()
            |> List.wrap()
        end
      after
        if notify? do
          notifications = Process.get(:ash_notifications, [])
          remaining_notifications = Ash.Notifier.notify(notifications)
          Process.delete(:ash_notifications) || []

          Ash.Actions.Helpers.warn_missed!(resource, action, %{
            resource_notifications: remaining_notifications
          })

          Process.delete(:ash_started_transaction?)
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
        base_changeset,
        must_return_records_for_changes?,
        changes,
        must_be_simple_results
      )
      |> then(fn tagged_results ->
        # Invalid changesets already had hooks run, tagged as :ok_hooks_done/:error_hooks_done
        invalid_changeset_results ++ tagged_results
      end)
      |> process_results(
        opts,
        ref,
        domain,
        resource,
        base_changeset
      )
      |> Stream.concat(must_be_simple_results)
      |> then(fn stream ->
        if opts[:return_stream?] do
          stream
          |> Stream.map(fn
            {:error, _} = error_tuple -> error_tuple
            record -> {:ok, record}
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
         _base_changeset,
         must_return_records_for_changes?,
         changes,
         _must_be_simple_results
       ) do
    must_return_records? =
      opts[:notify?] ||
        Enum.any?(batch, fn item ->
          item.after_action != [] ||
            item.after_transaction != []
        end)

    # Can return both valid and invalid changesets
    batch =
      Ash.Actions.Update.Bulk.run_bulk_before_batches(
        batch,
        changes,
        all_changes,
        opts,
        ref,
        :bulk_destroy
      )

    run_batch(
      resource,
      batch,
      action,
      opts,
      must_return_records?,
      must_return_records_for_changes?,
      domain,
      ref
    )
    |> run_after_action_hooks(opts, domain, ref)
    |> then(
      &Ash.Actions.Update.Bulk.run_bulk_after_changes(
        changes,
        all_changes,
        &1,
        batch,
        opts,
        ref,
        resource,
        :bulk_destroy_index
      )
    )
  end

  defp setup_changeset(
         {record, index},
         action,
         opts,
         input,
         argument_names,
         domain
       ) do
    record
    |> Ash.Changeset.new()
    |> Map.put(:domain, domain)
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
    |> Ash.Changeset.set_private_arguments_for_action(opts[:private_arguments] || %{})
    |> Ash.Changeset.put_context(:bulk_destroy, %{index: index, ref: make_ref()})
    |> Ash.Changeset.set_context(opts[:context] || %{})
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
      ash_context = Ash.ProcessHelpers.get_context_for_transfer(opts)

      Task.async_stream(
        stream,
        fn batch ->
          try do
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
                    Ash.Notifier.notify(bulk_notifications)
                    Ash.Notifier.notify(process_notifications)
                  end

                  []
                end
              end

            {batch_result, notifications, Process.get({:any_success?, ref})}
          after
            Process.delete(:ash_started_transaction?)
          end
        end,
        timeout: :infinity,
        max_concurrency: max_concurrency
      )
      |> Stream.flat_map(fn
        {:ok, {:throw, value}} ->
          throw(value)

        {:ok, {result, notifications, any_success?}} ->
          Process.put({:any_success?, ref}, any_success?)
          store_notification(ref, notifications, opts)
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

  defp index_changesets(batch) do
    Enum.reduce(batch, {%{}, %{}}, fn changeset, {by_ref, by_index} ->
      ref = changeset.context.bulk_destroy.ref
      index = changeset.context.bulk_destroy.index

      {
        Map.put(by_ref, ref, changeset),
        Map.put(by_index, index, ref)
      }
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
      {result.error_count + 1, [error | List.wrap(result.errors)]}
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

  defp store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_notification(ref, notification, opts) do
    if opts[:notify?] || opts[:return_notifications?] do
      notifications = Process.get({:bulk_notifications, ref}) || []

      new_notifications =
        if is_list(notification) do
          notification ++ List.wrap(notifications)
        else
          [notification | List.wrap(notifications)]
        end

      Process.put({:bulk_notifications, ref}, new_notifications)
    end
  end

  defp authorize(batch, opts) do
    if opts[:authorize?] do
      batch
      |> Enum.map(fn changeset ->
        if changeset.valid? do
          case Ash.can(
                 changeset,
                 opts[:actor],
                 return_forbidden_error?: true,
                 pre_flight?: false,
                 maybe_is: false
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

  defp handle_bulk_result(%Ash.BulkResult{} = bulk_result, _resource, _action, opts) do
    bulk_result
    |> sort(opts)
    |> ensure_records_return_type(opts)
    |> ensure_errors_return_type(opts)
  end

  # for when we return a stream
  defp handle_bulk_result(stream, _, _, _), do: stream

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

  defp sort(%{records: records} = result, opts) when is_list(records) do
    if opts[:sorted?] do
      %{result | records: Enum.sort_by(records, & &1.__metadata__.bulk_destroy_index)}
    else
      result
    end
  end

  defp sort(result, _), do: result

  defp run_batch(
         resource,
         batch,
         action,
         opts,
         must_return_records?,
         must_return_records_for_changes?,
         domain,
         ref
       ) do
    batch =
      Enum.map(batch, fn changeset ->
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

    # Split valid/invalid changesets after before_actions processing
    # Invalid changesets become {:error, error, changeset} tuples
    # after_transaction hooks for these will run later in process_results
    {batch, invalid_changeset_errors} =
      Ash.Actions.Helpers.split_valid_invalid_changesets(batch, opts)

    # Build index maps from valid changesets only
    {changesets_by_ref, changesets_by_index} = index_changesets(batch)

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

                if function_exported?(mod, :bulk_destroy, 3) do
                  mod.bulk_destroy(batch, manual_opts, %Ash.Resource.ManualDestroy.BulkContext{
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
                  ctx =
                    %Ash.Resource.ManualDestroy.Context{
                      select: opts[:select],
                      source_context: source_context,
                      actor: opts[:actor],
                      tenant: opts[:tenant],
                      authorize?: opts[:authorize?],
                      tracer: opts[:tracer],
                      return_notifications?: opts[:return_notifications?] || false,
                      return_destroyed?:
                        opts[:return_records?] || must_return_records? ||
                          must_return_records_for_changes?,
                      domain: domain
                    }

                  [changeset] = batch

                  [
                    mod.destroy(changeset, manual_opts, ctx)
                    |> Ash.Actions.BulkManualActionHelpers.process_non_bulk_result(
                      changeset,
                      :bulk_destroy,
                      &store_notification/3,
                      ref,
                      opts
                    )
                  ]
                end
                |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                |> Ash.Actions.BulkManualActionHelpers.process_bulk_results(
                  mod,
                  :bulk_destroy,
                  &store_notification/3,
                  ref,
                  opts,
                  batch,
                  changesets_by_ref,
                  changesets_by_index
                )

              _ ->
                [changeset] = batch

                result =
                  resource
                  |> Ash.DataLayer.destroy(changeset)
                  |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)

                case result do
                  :ok ->
                    {:ok, data} = Ash.Changeset.apply_attributes(changeset, force?: true)

                    {:ok,
                     [
                       data
                       |> Ash.Resource.set_meta(%Ecto.Schema.Metadata{
                         state: :deleted,
                         schema: changeset.resource
                       })
                       |> Ash.Resource.put_metadata(
                         :bulk_destroy_index,
                         changeset.context.bulk_destroy.index
                       )
                       |> Ash.Resource.put_metadata(
                         :bulk_action_ref,
                         changeset.context.bulk_destroy.ref
                       )
                     ]}

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

            {:error, %Ash.Error.Changes.StaleRecord{}} ->
              []

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
              Ash.Actions.Helpers.lookup_changeset(
                result,
                changesets_by_ref,
                changesets_by_index,
                index_key: :bulk_destroy_index,
                ref_key: :bulk_action_ref
              )

            [{:ok, result, changeset}]

          {:error, error} ->
            # Attach error to all changesets in the batch
            batch |> Enum.map(&{:error, error, &1})
        end)
        |> Enum.concat(invalid_changeset_errors)
    end
  end

  defp manage_relationships(destroyed, domain, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(domain, destroyed, changeset, engine_opts),
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

  defp run_after_action_hooks(batch_results, opts, domain, ref) do
    # batch_results is now list of {:ok, result, changeset} | {:error, error, changeset}
    # Changeset is already embedded in tuples from run_batch
    Enum.flat_map(batch_results, fn
      {:ok, result, changeset} ->
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

                [{:error, error, changeset}]

              {:ok, result, changeset, %{notifications: more_new_notifications}} ->
                store_notification(ref, more_new_notifications, opts)
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

  defp process_results(
         tagged_results,
         opts,
         ref,
         domain,
         resource,
         base_changeset
       ) do
    # tagged_results is now list of {:ok, result, changeset} | {:error, error, changeset}
    # Also handles {:ok_hooks_done, result, changeset} | {:error_hooks_done, error, changeset}
    # where after_transaction hooks have already been run
    # run_bulk_after_changes has already been called in do_handle_batch (inside transaction)
    results =
      Enum.flat_map(tagged_results, fn
        {:ok, result, changeset} ->
          if opts[:notify?] || opts[:return_notifications?] do
            store_notification(ref, notification(changeset, result, opts), opts)
          end

          try do
            case Ash.Changeset.run_after_transactions({:ok, result}, changeset) do
              {:ok, result} ->
                Process.put({:any_success?, ref}, true)

                if opts[:return_records?] do
                  metadata =
                    if index = changeset.context[:bulk_destroy][:index] do
                      %{bulk_destroy_index: index}
                    else
                      %{}
                    end

                  [Ash.Resource.set_metadata(result, metadata)]
                else
                  []
                end

              {:error, error} ->
                error
                |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                |> Ash.Helpers.error()
                |> List.wrap()
            end
          rescue
            e ->
              e
              |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
              |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
              |> Ash.Helpers.error()
              |> List.wrap()
          end

        {:error, error, changeset} ->
          try do
            case Ash.Changeset.run_after_transactions({:error, error}, changeset) do
              {:ok, result} ->
                # after_transaction converted error to success
                Process.put({:any_success?, ref}, true)

                if opts[:notify?] || opts[:return_notifications?] do
                  store_notification(ref, notification(changeset, result, opts), opts)
                end

                if opts[:return_records?] do
                  metadata =
                    if index = changeset.context[:bulk_destroy][:index] do
                      %{bulk_destroy_index: index}
                    else
                      %{}
                    end

                  [Ash.Resource.set_metadata(result, metadata)]
                else
                  []
                end

              {:error, error} ->
                error
                |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
                |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
                |> Ash.Helpers.error()
                |> List.wrap()
            end
          rescue
            e ->
              e
              |> Ash.Actions.Helpers.Bulk.maybe_rollback(resource, opts)
              |> Ash.Actions.Helpers.Bulk.maybe_stop_on_error(opts)
              |> Ash.Helpers.error()
              |> List.wrap()
          end

        {:ok_hooks_done, result, changeset} ->
          # after_transaction hooks already ran and converted error to success
          Process.put({:any_success?, ref}, true)

          if opts[:notify?] || opts[:return_notifications?] do
            store_notification(ref, notification(changeset, result, opts), opts)
          end

          if opts[:return_records?] do
            metadata =
              if index = changeset.context[:bulk_destroy][:index] do
                %{bulk_destroy_index: index}
              else
                %{}
              end

            [Ash.Resource.set_metadata(result, metadata)]
          else
            []
          end

        {:error_hooks_done, error, _changeset} ->
          # maybe_rollback and maybe_stop_on_error already called when tag was created
          [{:error, error}]
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

    # Combine loaded records with errors
    loaded_records ++ errors
  end

  # Helper for notification dispatch (extracted from inline code for reuse)
  defp handle_atomic_notifications(bulk_result, resource, action, notify?, opts) do
    if opts[:return_notifications?] do
      bulk_result
    else
      if notify? do
        notifications =
          List.wrap(Process.delete(:ash_notifications)) ++
            List.wrap(bulk_result.notifications)

        if opts[:notify?] do
          remaining_notifications = Ash.Notifier.notify(notifications)

          Ash.Actions.Helpers.warn_missed!(resource, action, %{
            resource_notifications: remaining_notifications
          })

          %{bulk_result | notifications: notifications}
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

  defp notification(changeset, result, opts) do
    Ash.Actions.Helpers.resource_notification(changeset, result, opts)
  end
end
