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
            tenant: opts[:tenant]
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

        changeset = opts[:atomic_changeset] ->
          changeset

        query.action.manual ->
          {:not_atomic, "Manual read actions cannot be updated atomically"}

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
            read_opts =
              opts
              |> then(fn read_opts ->
                if opts[:batch_size] do
                  Keyword.put(read_opts, :batch_size, opts[:stream_batch_size])
                else
                  read_opts
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
              read_opts = Keyword.take(read_opts, Ash.stream_opt_keys())

              # We need to figure out a way to capture errors raised by the stream when picking items off somehow
              # for now, we only go this route if there are potentially more records in the result set than
              # in the batch size, to solve this problem for atomic upgrades.
              # we can likely make the stream throw something instead of raising somethign
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

      %Ash.Changeset{valid?: false, errors: errors} ->
        %Ash.BulkResult{
          status: :error,
          error_count: 1,
          errors: [Ash.Error.to_error_class(errors)]
        }

      atomic_changeset ->
        {atomic_changeset, opts} =
          Ash.Actions.Helpers.set_context_and_get_opts(domain, atomic_changeset, opts)

        atomic_changeset = Ash.Actions.Helpers.apply_opts_load(atomic_changeset, opts)

        atomic_changeset = %{atomic_changeset | domain: domain}

        atomic_changeset =
          if opts[:context] do
            Ash.Changeset.set_context(atomic_changeset, opts[:context])
          else
            atomic_changeset
          end

        notify? =
          if Process.get(:ash_started_transaction?) do
            false
          else
            Process.put(:ash_started_transaction?, true)
            true
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
              action.changes ++ Ash.Resource.Info.changes(atomic_changeset.resource, :destroy),
              fn
                %{change: {module, change_opts}} ->
                  module.has_after_batch?() &&
                    module.batch_callbacks?(query, change_opts, context)

                _ ->
                  false
              end
            )

          opts =
            if has_after_batch_hooks? || !Enum.empty?(atomic_changeset.after_action) ||
                 opts[:notify?] do
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

          if (has_after_batch_hooks? || !Enum.empty?(atomic_changeset.after_action)) &&
               Keyword.get(opts, :transaction, true) do
            Ash.DataLayer.transaction(
              List.wrap(atomic_changeset.resource) ++ action.touches_resources,
              fn ->
                do_atomic_destroy(query, atomic_changeset, has_after_batch_hooks?, input, opts)
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
            {:ok, do_atomic_destroy(query, atomic_changeset, has_after_batch_hooks?, input, opts)}
          end
          |> case do
            {:ok, bulk_result} ->
              if opts[:return_notifications?] do
                bulk_result
              else
                if notify? do
                  notifications =
                    List.wrap(Process.delete(:ash_notifications)) ++
                      List.wrap(bulk_result.notifications)

                  if opts[:notify?] do
                    remaining_notifications = Ash.Notifier.notify(notifications)

                    Ash.Actions.Helpers.warn_missed!(atomic_changeset.resource, action, %{
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

            {:error, error} ->
              %Ash.BulkResult{
                status: :error,
                errors: [Ash.Error.to_ash_error(error)],
                error_count: 1
              }
          end
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
          }
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

  defp do_atomic_destroy(query, atomic_changeset, has_after_batch_hooks?, input, opts) do
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

    destroy_query_opts =
      opts
      |> Keyword.take([:return_records?, :tenant, :select])
      |> Map.new()
      |> Map.put(:calculations, calculations)
      |> Map.put(
        :action_select,
        action_select
      )

    with {:ok, query} <-
           authorize_bulk_query(query, atomic_changeset, opts),
         {:ok, atomic_changeset, query} <-
           authorize_atomic_changeset(query, atomic_changeset, opts),
         {query, atomic_changeset} <- add_changeset_filters(query, atomic_changeset),
         {:ok, data_layer_query} <- Ash.Query.data_layer_query(query) do
      case Ash.DataLayer.destroy_query(
             data_layer_query,
             atomic_changeset,
             destroy_query_opts
           )
           |> Ash.Actions.Helpers.rollback_if_in_transaction(query.resource, nil) do
        :ok ->
          %Ash.BulkResult{
            status: :success
          }

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

          {results, errors, error_count, notifications} =
            case load_data(
                   results,
                   atomic_changeset.domain,
                   atomic_changeset.resource,
                   atomic_changeset,
                   opts
                 ) do
              {:ok, results} ->
                if Enum.empty?(atomic_changeset.after_action) do
                  {results, [], 0, notifications}
                else
                  Enum.reduce(results, {[], [], 0, notifications}, fn result,
                                                                      {results, errors,
                                                                       error_count,
                                                                       notifications} ->
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

                        {results, errors ++ List.wrap(error),
                         error_count + Enum.count(List.wrap(error)), notifications}

                      {:ok, result, _changeset, %{notifications: more_new_notifications}} ->
                        {[result | results], errors, error_count,
                         notifications ++ more_new_notifications}
                    end
                  end)
                  |> then(fn {results, errors, error_count, notifications} ->
                    {Enum.reverse(results), errors, error_count, notifications}
                  end)
                end

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
      {:error, %Ash.Error.Forbidden.InitialDataRequired{}} ->
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
        %Ash.BulkResult{
          status: :error,
          error_count: 1,
          errors: [Ash.Error.to_error_class(error)]
        }
    end
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

  defp add_changeset_filters(query, changeset) do
    {Ash.Query.do_filter(query, changeset.filter), %{changeset | filter: nil}}
  end

  defp do_run(domain, stream, action, input, opts, not_atomic_reason) do
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
              opts,
              not_atomic_reason
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

  defp do_atomic_batches(atomic_changeset, domain, stream, action, input, opts, not_atomic_reason) do
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

        read_action = get_read_action(resource, opts).name

        resource
        |> Ash.Query.for_read(read_action, %{},
          actor: opts[:actor],
          authorize?: false,
          context: atomic_changeset.context,
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
            [
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
              return_records?: opts[:return_records?],
              allow_stream_with: opts[:allow_stream_with],
              strategy: [:atomic]
            ],
            not_atomic_reason
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
              store_error(ref, errors, opts, error_count)
              []
          end
        end)
      end
    )
    |> run_batches(ref, opts)
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
            Ash.Notifier.notify(Process.delete({:bulk_destroy_notifications, ref}))
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
            nil
          end

        notifications =
          if opts[:notify?] && opts[:return_notifications?] do
            Process.delete({:bulk_destroy_notifications, ref})
          else
            if opts[:notify?] do
              Ash.Notifier.notify(Process.delete({:bulk_destroy_notifications, ref}))
            else
              []
            end
          end

        notifications =
          if Process.get(:ash_started_transaction?) && !opts[:return_notifications?] do
            Process.put(:ash_notifications, Process.get(:ash_notifications, []) ++ notifications)
            []
          else
            notifications
          end

        {errors, error_count} = Process.get({:bulk_destroy_errors, ref}) || {[], 0}

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
            notifications: Process.delete({:bulk_destroy_notifications, ref})
          }

          {error_count, errors} = errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_destroy_errors, ref})
        Process.delete({:bulk_destroy_notifications, ref})
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
    |> Ash.Changeset.set_arguments(arguments)
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

  defp pre_template_all_changes(action, resource, :destroy, base, actor) do
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
    Stream.resource(
      fn -> Process.delete({:bulk_destroy_errors, ref}) end,
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
    Stream.resource(
      fn -> Process.delete({:bulk_destroy_notifications, ref}) end,
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

    {batch, must_be_simple} =
      Enum.reduce(batch, {[], []}, fn changeset, {batch, must_be_simple} ->
        if changeset.around_transaction in [[], nil] and changeset.after_transaction in [[], nil] and
             changeset.around_action in [[], nil] do
          changeset = Ash.Changeset.run_before_transaction_hooks(changeset)
          {[changeset | batch], must_be_simple}
        else
          {batch, [%{changeset | __validated_for_action__: action.name} | must_be_simple]}
        end
      end)

    must_be_simple_results =
      Enum.flat_map(must_be_simple, fn changeset ->
        case Ash.Actions.Destroy.run(
               domain,
               changeset,
               action,
               Keyword.put(opts, :return_destroyed?, opts[:return_records?])
             ) do
          :ok ->
            Process.put({:any_success?, ref}, true)

            []

          {:ok, result} when not is_list(result) ->
            Process.put({:any_success?, ref}, true)

            [
              Ash.Resource.set_metadata(result, %{
                bulk_destroy_index: changeset.context.bulk_destroy.index
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
                base_changeset,
                must_return_records_for_changes?,
                changes,
                must_be_simple_results
              )

            {new_errors, new_error_count} =
              Process.delete({:bulk_destroy_errors, tmp_ref}) || {[], 0}

            store_error(ref, new_errors, opts, new_error_count)

            notifications = Process.get({:bulk_destroy_notifications, tmp_ref}) || []
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
      |> Enum.to_list()
      |> run_bulk_before_batches(
        changes,
        all_changes,
        opts,
        ref
      )

    changesets_by_index = index_changesets(batch)

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
    |> run_after_action_hooks(opts, domain, ref, changesets_by_index)
    |> process_results(
      changes,
      all_changes,
      opts,
      ref,
      changesets_by_index,
      batch,
      domain,
      resource,
      base_changeset
    )
    |> Stream.concat(must_be_simple_results)
    |> then(fn stream ->
      if opts[:return_stream?] do
        stream
        |> Stream.map(&{:ok, &1})
        |> Stream.concat(error_stream(ref))
        |> Stream.concat(notification_stream(ref))
      else
        stream
      end
    end)
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
    |> Ash.Changeset.put_context(:bulk_destroy, %{index: index})
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
            {errors, _} = Process.get({:bulk_destroy_errors, ref}) || {[], 0}

            notifications =
              if opts[:notify?] do
                process_notifications = Process.get(:ash_notifications, [])
                bulk_notifications = Process.get({:bulk_destroy_notifications, ref}) || []

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

            {batch_result, notifications, errors, Process.get({:any_success?, ref})}
          after
            Process.delete(:ash_started_transaction?)
          end
        end,
        timeout: :infinity,
        max_concurrency: max_concurrency
      )
      |> Stream.map(fn
        {:ok, {:throw, value}} ->
          throw(value)

        {:ok, {result, notifications, errors, any_success?}} ->
          Process.put({:any_success?, ref}, any_success?)
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

  defp index_changesets(batch) do
    Enum.reduce(batch, %{}, fn changeset, changesets_by_index ->
      Map.put(
        changesets_by_index,
        changeset.context.bulk_destroy.index,
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

  defp run_bulk_before_batches(
         batch,
         changes,
         all_changes,
         opts,
         ref
       ) do
    all_changes
    |> Enum.filter(fn
      {%{change: {module, _opts}}, _} ->
        module.has_before_batch?()

      _ ->
        false
    end)
    |> Enum.reduce(batch, fn {%{change: {module, change_opts}}, index}, batch ->
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
              changeset.context.bulk_destroy.index in List.wrap(changes[index])
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

  defp store_error(ref, empty, _opts, error_count) when empty in [[], nil] do
    {errors, count} = Process.get({:bulk_destroy_errors, ref}) || {[], 0}
    Process.put({:bulk_destroy_errors, ref}, {errors, count + error_count})
  end

  defp store_error(ref, error, opts, count) do
    add = count || Enum.count(List.wrap(error))

    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error), 0, []})
    else
      if opts[:return_errors?] do
        {errors, count} = Process.get({:bulk_destroy_errors, ref}) || {[], 0}

        new_errors =
          error
          |> List.wrap()
          |> Enum.map(&Ash.Error.to_ash_error/1)

        Process.put(
          {:bulk_destroy_errors, ref},
          {new_errors ++ errors, count + add}
        )
      else
        {errors, count} = Process.get({:bulk_destroy_errors, ref}) || {[], 0}
        Process.put({:bulk_destroy_errors, ref}, {errors, count + add})
      end
    end
  end

  defp store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_notification(ref, notification, opts) do
    if opts[:notify?] || opts[:return_notifications?] do
      notifications = Process.get({:bulk_destroy_notifications, ref}) || []

      new_notifications =
        if is_list(notification) do
          notification ++ List.wrap(notifications)
        else
          [notification | List.wrap(notifications)]
        end

      Process.put({:bulk_destroy_notifications, ref}, new_notifications)
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
    batch
    |> Enum.map(fn changeset ->
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
              {mod, manual_opts} ->
                if function_exported?(mod, :bulk_destroy, 3) do
                  mod.bulk_destroy(batch, manual_opts, %{
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
                  })
                  |> Enum.flat_map(fn
                    {:ok, result} ->
                      [result]

                    {:error, error} ->
                      store_error(ref, error, opts)
                      []

                    {:notifications, notifications} ->
                      store_notification(ref, notifications, opts)
                      []
                  end)
                else
                  [changeset] = batch

                  result =
                    mod.destroy(changeset, opts, %Ash.Resource.ManualDestroy.Context{
                      select: opts[:select],
                      actor: opts[:actor],
                      tenant: opts[:tenant],
                      authorize?: opts[:authorize?],
                      tracer: opts[:tracer],
                      domain: domain
                    })

                  case result do
                    {:ok, result} ->
                      {:ok,
                       [
                         Ash.Resource.put_metadata(
                           result,
                           :bulk_destroy_index,
                           changeset.context.bulk_destroy.index
                         )
                       ]}

                    {:error, error} ->
                      {:error, error}
                  end
                end

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
                     ]}

                  {:error, error} ->
                    {:error, error}
                end
            end

          case result do
            {:error, %Ash.Error.Changes.StaleRecord{}} ->
              []

            {:ok, result} ->
              Process.put({:any_success?, ref}, true)

              result

            {:error, error} ->
              store_error(ref, error, opts)

              []
          end
        end)
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

  defp run_after_action_hooks(
         batch_results,
         opts,
         domain,
         ref,
         changesets_by_index
       ) do
    Enum.flat_map(batch_results, fn result ->
      changeset = changesets_by_index[result.__metadata__.bulk_destroy_index]

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
  end

  defp process_results(
         batch,
         changes,
         all_changes,
         opts,
         ref,
         changesets_by_index,
         changesets,
         domain,
         resource,
         base_changeset
       ) do
    changes
    |> Ash.Actions.Update.Bulk.run_bulk_after_changes(
      all_changes,
      batch,
      changesets_by_index,
      changesets,
      opts,
      ref,
      resource,
      :bulk_destroy_index
    )
    |> Enum.flat_map(fn result ->
      changeset = changesets_by_index[result.__metadata__[:bulk_destroy_index]]

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
           tenant: opts[:tenant],
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
    %Ash.Notifier.Notification{
      resource: changeset.resource,
      domain: changeset.domain,
      actor: opts[:actor],
      action: changeset.action,
      for: Ash.Resource.Info.notifiers(changeset.resource) ++ changeset.action.notifiers,
      data: result,
      changeset: changeset
    }
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
