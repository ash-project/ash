defmodule Ash.Actions.Update.Bulk do
  @moduledoc false

  require Ash.Query

  @spec run(Ash.Api.t(), Enumerable.t() | Ash.Query.t(), atom(), input :: map, Keyword.t()) ::
          Ash.BulkResult.t()

  def run(api, resource, action, input, opts, not_atomic_reason \\ nil)

  def run(api, resource, action, input, opts, not_atomic_reason) when is_atom(resource) do
    run(api, Ash.Query.new(resource), action, input, opts, not_atomic_reason)
  end

  def run(api, %Ash.Query{} = query, action, input, opts, not_atomic_reason) do
    opts = set_strategy(opts, query.resource)

    query =
      if query.action do
        query
      else
        query =
          Ash.Query.for_read(
            query,
            Ash.Resource.Info.primary_action!(query.resource, :read).name
          )

        {query, _opts} = Ash.Actions.Helpers.add_process_context(api, query, opts)

        query
      end

    query = %{query | api: api}

    fully_atomic_changeset =
      cond do
        not_atomic_reason ->
          {:not_atomic, not_atomic_reason}

        :atomic not in opts[:strategy] ->
          {:not_atomic, "Not in requested strategies"}

        changeset = opts[:atomic_changeset] ->
          changeset

        Ash.DataLayer.data_layer_can?(query.resource, :update_query) ->
          Ash.Changeset.fully_atomic_changeset(query.resource, action, input, opts)

        true ->
          {:not_atomic, "data layer does not support updating a query"}
      end

    case fully_atomic_changeset do
      {:not_atomic, reason} ->
        read_opts =
          Keyword.take(opts, Ash.Api.stream_opt_keys())

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
                    action: action.name,
                    requested_strategies: opts[:strategy],
                    not_stream_reason: "could not stream the query",
                    footer: "Non stream reason:\n\n" <> Exception.message(exception)
                  )
                )
              ]
            }

          _ ->
            run(
              api,
              api.stream!(
                query,
                Keyword.put(read_opts, :authorize?, opts[:authorize?] && opts[:authorize_query?])
              ),
              action,
              input,
              Keyword.put(opts, :resource, query.resource),
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
        {atomic_changeset, opts} =
          Ash.Actions.Helpers.add_process_context(api, atomic_changeset, opts)

        atomic_changeset = %{atomic_changeset | api: api}

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
          has_after_action_hooks? = not Enum.empty?(atomic_changeset.after_action)
          # There are performance implications here. We probably need to explicitly enable
          # having after action hooks. Or perhaps we need to stream the ids and then bulk update
          # them.
          opts =
            if has_after_action_hooks? || opts[:notify?] do
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

          bulk_result =
            if has_after_action_hooks? && opts[:transaction] do
              Ash.DataLayer.transaction(
                List.wrap(atomic_changeset.resource) ++ action.touches_resources,
                fn ->
                  do_atomic_update(query, atomic_changeset, has_after_action_hooks?, input, opts)
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
              do_atomic_update(query, atomic_changeset, has_after_action_hooks?, input, opts)
            end

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
        after
          if notify? do
            Process.put(:ash_started_transaction?, false)
          end
        end
    end
  end

  def run(api, stream, action, input, opts, not_atomic_reason) do
    not_atomic_reason = not_atomic_reason || "Cannot perform atomic updates on a stream of inputs"

    resource =
      opts[:resource] ||
        case stream do
          [%resource{} | _] ->
            resource

          _ ->
            nil
        end

    opts = Keyword.put(opts, :resource, resource)

    if !resource do
      raise ArgumentError,
            "Could not determine resource for bulk #{action.type}. Please provide the `resource` option if providing a stream of inputs."
    end

    opts = set_strategy(opts, resource)

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
          do_run(api, stream, action, input, opts, metadata_key, context_key, not_atomic_reason)
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
      api
      |> do_run(stream, action, input, opts, metadata_key, context_key, not_atomic_reason)
      |> handle_bulk_result(metadata_key, opts)
    end
  end

  defp do_atomic_update(query, atomic_changeset, has_after_action_hooks?, input, opts) do
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

    with {:ok, query} <-
           authorize_bulk_query(query, atomic_changeset, opts),
         {:ok, atomic_changeset, query} <-
           authorize_atomic_changeset(query, atomic_changeset, opts),
         {:ok, data_layer_query} <- Ash.Query.data_layer_query(query) do
      case Ash.DataLayer.update_query(
             data_layer_query,
             atomic_changeset,
             Map.new(Keyword.take(opts, [:return_records?, :tenant]))
           ) do
        :ok ->
          %Ash.BulkResult{
            status: :success
          }

        {:ok, results} ->
          results =
            case results do
              [result] ->
                if atomic_changeset.context[:data_layer][:use_atomic_update_data] do
                  Map.put(result, :__metadata__, atomic_changeset.data.__metadata__)
                else
                  [result]
                end

              other ->
                other
            end

          results = List.wrap(results)

          {errors, results, notifications, error_count} =
            if has_after_action_hooks? do
              results
              |> Enum.reduce({[], [], [], 0}, fn
                result, {errors, successes, notifications, error_count} ->
                  case Ash.Changeset.run_after_actions(result, atomic_changeset, []) do
                    {:error, error} ->
                      {[error | errors], successes, error_count + 1}

                    {:ok, result, _changeset, %{notifications: new_notifications}} ->
                      {errors, [result | successes], notifications ++ new_notifications,
                       error_count}
                  end
              end)
              |> then(fn {errors, successes, notifications, error_count} ->
                {Enum.reverse(errors), Enum.reverse(successes), notifications, error_count}
              end)
            else
              {[], results, [], 0}
            end

          {results, errors, error_count} =
            case load_data(
                   results,
                   atomic_changeset.api,
                   atomic_changeset.resource,
                   atomic_changeset,
                   opts
                 ) do
              {:ok, results} ->
                {results, errors, error_count}

              {:error, error} ->
                {[], List.wrap(error) ++ errors, error_count + Enum.count(List.wrap(error))}
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

              {0, _results} ->
                :success
            end

          %Ash.BulkResult{
            status: status,
            error_count: error_count,
            notifications: notifications,
            errors: errors,
            records: results
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
              atomic_changeset.api,
              query,
              atomic_changeset.action,
              input,
              opts,
              "authorization requires initial data"
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

  defp set_strategy(opts, resource) do
    if Ash.DataLayer.data_layer_can?(resource, :update_query) do
      opts
    else
      Keyword.put(opts, :strategy, [:stream])
    end
  end

  defp do_run(api, stream, action, input, opts, metadata_key, context_key, not_atomic_reason) do
    resource = opts[:resource]
    opts = Ash.Actions.Helpers.set_opts(opts, api)

    {_, opts} = Ash.Actions.Helpers.add_process_context(api, Ash.Changeset.new(resource), opts)

    fully_atomic_changeset =
      cond do
        :atomic_batches not in opts[:strategy] ->
          {:not_atomic, "Not in requested strategies"}

        Enum.empty?(Ash.Resource.Info.primary_key(resource)) ->
          {:not_atomic, "cannot atomically update a stream without a primary key"}

        !Ash.Resource.Info.primary_action(resource, :read) ->
          {:not_atomic, "cannot atomically update a stream without a primary read action"}

        Ash.DataLayer.data_layer_can?(resource, :update_query) ->
          Ash.Changeset.fully_atomic_changeset(resource, action, input, opts)

        true ->
          {:not_atomic, "data layer does not support updating a query"}
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
              do_stream_batches(api, stream, action, input, opts, metadata_key, context_key)
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
              api,
              stream,
              action,
              input,
              opts
            )
        end

      {:not_atomic, not_atomic_batches_reason} ->
        if :stream in opts[:strategy] do
          do_stream_batches(api, stream, action, input, opts, metadata_key, context_key)
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
         api,
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
        |> Ash.Query.for_read(Ash.Resource.Info.primary_action!(resource, :read).name, %{},
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
          run(api, query, action.name, input,
            actor: opts[:actor],
            authorize?: false,
            tenant: atomic_changeset.tenant,
            tracer: opts[:tracer],
            atomic_changeset: atomic_changeset,
            return_errors?: opts[:return_errors?],
            return_notifications?: opts[:return_notifications?],
            notify?: opts[:notify?],
            return_records?: opts[:return_records?],
            strategy: opts[:strategy]
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
              {:error, Ash.Error.to_error_class(errors)}
          end
        end)
      end
    )
    |> run_batches(ref, opts)
  end

  defp do_stream_batches(api, stream, action, input, opts, metadata_key, context_key) do
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

    base_changeset = base_changeset(resource, api, opts, action, input)

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
              api,
              context_key
            )
          )
          |> reject_and_maybe_store_errors(ref, opts)
          |> handle_batch(
            api,
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
      case query.api.can(query, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
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
      case changeset.api.can(changeset, opts[:actor],
             return_forbidden_error?: true,
             maybe_is: false,
             atomic_changeset: changeset,
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

  defp base_changeset(resource, api, opts, action, input) do
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
    |> Map.put(:api, api)
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

  defp pre_template(opts, changeset, actor) do
    if Ash.Filter.template_references_context?(opts) do
      opts
    else
      {:templated,
       Ash.Filter.build_filter_from_template(
         opts,
         actor,
         changeset.arguments,
         changeset.context
       )}
    end
  end

  defp error_stream(ref) do
    Stream.resource(
      fn -> Process.delete({:bulk_update_errors, ref}) end,
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
      fn -> Process.delete({:bulk_update_notifications, ref}) end,
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
         api,
         resource,
         action,
         all_changes,
         opts,
         ref,
         context_key,
         metadata_key,
         base_changeset
       ) do
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
            {_starting_errors, starting_error_count} =
              Process.get({:bulk_update_errors, ref}) || {[], 0}

            result =
              do_handle_batch(
                batch,
                api,
                resource,
                action,
                opts,
                all_changes,
                ref,
                metadata_key,
                context_key,
                base_changeset
              )

            {new_errors, new_error_count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

            if new_error_count != starting_error_count do
              Ash.DataLayer.rollback(
                resource,
                {:error, Enum.take(new_errors, new_error_count - starting_error_count)}
              )
            else
              result
            end
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
            [{:error, error}]
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
        api,
        resource,
        action,
        opts,
        all_changes,
        ref,
        metadata_key,
        context_key,
        base_changeset
      )
    end
  end

  defp do_handle_batch(
         batch,
         api,
         resource,
         action,
         opts,
         all_changes,
         ref,
         metadata_key,
         context_key,
         base_changeset
       ) do
    must_return_records? =
      opts[:notify?] ||
        Enum.any?(batch, fn item ->
          item.after_action != []
        end)

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

    batch =
      batch
      |> authorize(api, opts)
      |> run_bulk_before_batches(
        changes,
        all_changes,
        opts,
        ref,
        context_key
      )

    # TODO: We will likely need to store the changeset in record metadata after calling the
    # data layer instead of passing this in, as this is a stale changeset
    changesets_by_index = index_changesets(batch, context_key)

    run_batch(
      resource,
      batch,
      action,
      opts,
      must_return_records?,
      must_return_records_for_changes?,
      api,
      ref,
      metadata_key,
      context_key
    )
    |> run_after_action_hooks(opts, api, ref, changesets_by_index, metadata_key)
    |> process_results(
      changes,
      all_changes,
      opts,
      ref,
      changesets_by_index,
      metadata_key,
      resource,
      api,
      base_changeset
    )
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
         api,
         context_key
       ) do
    record
    |> Ash.Changeset.new()
    |> Map.put(:api, api)
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

  defp handle_params(changeset, false, action, _opts, input, _argument_names) do
    Ash.Changeset.handle_params(changeset, action, input)
  end

  defp handle_params(changeset, true, action, _opts, input, argument_names) do
    {args, attrs} =
      Map.split(input, argument_names)

    %{changeset | arguments: args, attributes: attrs}
    |> Ash.Changeset.handle_params(action, input, cast_params?: false)
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
        module.before_batch(batch, change_opts, %{
          actor: opts[:actor],
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        })
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
          module.before_batch(matches, change_opts, %{
            actor: opts[:actor],
            tenant: opts[:tenant],
            tracer: opts[:tracer],
            authorize?: opts[:authorize?]
          })

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

  defp reject_and_maybe_store_errors(stream, ref, opts) do
    Enum.reject(stream, fn changeset ->
      if changeset.valid? do
        false
      else
        store_error(ref, changeset, opts)
        true
      end
    end)
  end

  defp store_error(ref, errors, opts, count \\ nil)

  defp store_error(_ref, empty, _opts, 0) when empty in [[], nil], do: :ok

  defp store_error(ref, empty, _opts, error_count) when empty in [[], nil] do
    {errors, count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

    Process.put(
      {:bulk_update_errors, ref},
      {errors, count + error_count}
    )
  end

  defp store_error(ref, error, opts, count) do
    add = count || Enum.count(List.wrap(error))

    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error), 0, []})
    else
      if opts[:return_errors?] do
        {errors, count} = Process.get({:bulk_update_errors, ref}) || {[], 0}

        error =
          case error do
            %Ash.Changeset{} = changeset ->
              changeset

            other ->
              Ash.Error.to_ash_error(other)
          end

        Process.put(
          {:bulk_update_errors, ref},
          {[error | errors], count + add}
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

  defp authorize(batch, api, opts) do
    if opts[:authorize?] do
      batch
      |> Enum.map(fn changeset ->
        if changeset.valid? do
          case api.can(changeset, opts[:actor], return_forbidden_error?: true, maybe_is: false) do
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
         api,
         ref,
         metadata_key,
         context_key
       ) do
    batch
    |> Enum.map(fn changeset ->
      if changeset.valid? do
        {changeset, %{notifications: new_notifications}} =
          Ash.Changeset.run_before_actions(changeset)

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
        |> Enum.group_by(&{&1.atomics, &1.filters})
        |> Enum.flat_map(fn {_atomics, batch} ->
          result =
            case action.manual do
              {mod, opts} ->
                if function_exported?(mod, context_key, 3) do
                  apply(mod, context_key, [
                    batch,
                    opts,
                    %{
                      actor: opts[:actor],
                      batch_size: opts[:batch_size],
                      authorize?: opts[:authorize?],
                      tracer: opts[:tracer],
                      api: api,
                      return_records?:
                        opts[:return_records?] || must_return_records? ||
                          must_return_records_for_changes?,
                      tenant: opts[:tenant]
                    }
                  ])
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
                    apply(mod, action.type, [
                      changeset,
                      opts,
                      %{
                        actor: opts[:actor],
                        tenant: opts[:tenant],
                        authorize?: opts[:authorize?],
                        tracer: opts[:tracer],
                        api: api
                      }
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
                    case Ash.DataLayer.update(resource, changeset) do
                      {:ok, result} ->
                        result =
                          Ash.Resource.put_metadata(
                            result,
                            metadata_key,
                            changeset.context[context_key].index
                          )

                        {:cont, {:ok, results ++ [result]}}

                      {:error, error} ->
                        {:halt, {:error, error}}
                    end
                  end
                )
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
  end

  defp manage_relationships(updated, api, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(api, updated, changeset, engine_opts),
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
         api,
         ref,
         changesets_by_index,
         metadata_key
       ) do
    Enum.flat_map(batch_results, fn result ->
      changeset = changesets_by_index[result.__metadata__[metadata_key]]

      case manage_relationships(result, api, changeset,
             actor: opts[:actor],
             authorize?: opts[:authorize?]
           ) do
        {:ok, result, %{notifications: new_notifications, new_changeset: changeset}} ->
          store_notification(ref, new_notifications, opts)

          case Ash.Changeset.run_after_actions(result, changeset, []) do
            {:error, error} ->
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
         metadata_key,
         resource,
         api,
         base_changeset
       ) do
    run_bulk_after_changes(
      changes,
      all_changes,
      batch,
      changesets_by_index,
      opts,
      ref,
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
    |> load_data(api, resource, base_changeset, opts)
    |> case do
      {:ok, records} ->
        Enum.reject(records, & &1.__metadata__[:private][:missing_from_data_layer])

      {:error, error} ->
        store_error(ref, error, opts)
        []
    end
  end

  defp load_data(records, api, resource, changeset, opts) do
    select =
      if changeset.select do
        List.wrap(changeset.select)
      else
        resource |> Ash.Resource.Info.public_attributes() |> Enum.map(& &1.name)
      end

    case List.wrap(changeset.load) ++ select do
      [] ->
        {:ok, records}

      load ->
        api.load(
          records,
          load,
          actor: opts[:actor],
          authorize?: opts[:authorize?],
          tracer: opts[:tracer]
        )
    end
  end

  defp run_bulk_after_changes(
         changes,
         all_changes,
         results,
         changesets_by_index,
         opts,
         ref,
         metadata_key
       ) do
    all_changes
    |> Enum.filter(fn
      {%{change: {module, _opts}}, _} ->
        function_exported?(module, :after_batch, 3)

      _ ->
        false
    end)
    |> Enum.reduce(results, fn {%{change: {module, change_opts}}, index}, results ->
      if changes[index] == :all do
        results =
          Enum.map(results, fn result ->
            {changesets_by_index[result.__metadata__[metadata_key]], result}
          end)

        module.after_batch(results, change_opts, %{
          actor: opts[:actor],
          tenant: opts[:tenant],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        })
        |> handle_after_batch_results(ref, opts)
      else
        {matches, non_matches} =
          results
          |> Enum.split_with(fn
            {:ok, result} ->
              result.__metadata__[metadata_key] in List.wrap(changes[index])

            _ ->
              false
          end)

        matches =
          Enum.map(matches, fn match ->
            {changesets_by_index[match.__metadata__[metadata_key]], match}
          end)

        after_batch_results =
          module.after_batch(matches, change_opts, %{
            actor: opts[:actor],
            tenant: opts[:tenant],
            tracer: opts[:tracer],
            authorize?: opts[:authorize?]
          })
          |> handle_after_batch_results(ref, opts)

        Enum.concat([after_batch_results, non_matches])
      end
    end)
  end

  defp handle_after_batch_results(results, ref, options) do
    Enum.flat_map(
      results,
      fn
        %Ash.Notifier.Notification{} = notification ->
          store_notification(ref, notification, options)

        {:ok, result} ->
          [result]

        {:error, error} ->
          store_error(ref, error, options)
          []
      end
    )
  end

  defp notification(changeset, result, opts) do
    %Ash.Notifier.Notification{
      resource: changeset.resource,
      api: changeset.api,
      actor: opts[:actor],
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
              if Enum.all?(validation.where || [], fn {module, opts} ->
                   opts = templated_opts(opts, actor, changeset.arguments, changeset.context)
                   {:ok, opts} = module.init(opts)
                   module.validate(changeset, opts, context) == :ok
                 end) do
                opts = templated_opts(opts, actor, changeset.arguments, changeset.context)
                {:ok, opts} = module.init(opts)

                case module.validate(
                       changeset,
                       opts,
                       Map.put(context, :message, validation.message)
                     ) do
                  :ok ->
                    changeset

                  {:error, error} ->
                    Ash.Changeset.add_error(changeset, error)
                end
              else
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
                end) || function_exported?(module, :after_batch, 3)

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
                    module.validate(changeset, opts, context) == :ok
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
              context = %{
                actor: actor,
                authorize?: authorize? || false,
                tracer: tracer
              }

              matches = batch_change(module, matches, change_opts, context, actor)

              must_return_records? =
                state.must_return_records? ||
                  Enum.any?(batch, fn item ->
                    item.relationships not in [nil, %{}] || !Enum.empty?(item.after_action)
                  end) || function_exported?(module, :after_batch, 3)

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

            module.change(changeset, change_opts, Map.put(context, :bulk?, true))
          end)
        end

      change_opts ->
        Enum.map(batch, fn changeset ->
          change_opts = templated_opts(change_opts, actor, changeset.arguments, changeset.context)
          {:ok, change_opts} = module.init(change_opts)

          module.change(changeset, change_opts, Map.put(context, :bulk?, true))
        end)
    end
  end

  defp templated_opts({:templated, opts}, _actor, _arguments, _context), do: opts

  defp templated_opts(opts, actor, arguments, context) do
    Ash.Filter.build_filter_from_template(
      opts,
      actor,
      arguments,
      context
    )
  end
end
