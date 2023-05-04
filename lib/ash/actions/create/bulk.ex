defmodule Ash.Actions.Create.Bulk do
  @moduledoc false
  @spec run(Ash.Api.t(), Ash.Resource.t(), atom(), Enumerable.t(map), Keyword.t()) ::
          :ok
          | {:ok, [Ash.Resource.record()]}
          | {:ok, [Ash.Resource.record()], [Ash.Notifier.Notification.t()]}
          | {:error, term}
  def run(api, resource, action, inputs, opts) do
    action = Ash.Resource.Info.action(resource, action)

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

      Ash.DataLayer.transaction(
        List.wrap(resource) ++ action.touches_resources,
        fn ->
          do_run(api, resource, action, inputs, opts)
        end,
        opts[:timeout],
        %{
          type: :bulk_create,
          metadata: %{
            resource: resource,
            action: action.name,
            actor: opts[:actor]
          }
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

          handle_bulk_result(bulk_result, resource, action, opts)

        {:error, error} ->
          {:error, error}
      end
    else
      api
      |> do_run(resource, action, inputs, opts)
      |> handle_bulk_result(resource, action, opts)
    end
  end

  def do_run(api, resource, action, inputs, opts) do
    opts = Ash.Actions.Helpers.set_opts(opts, api)

    if action.manual? do
      raise "Old style manual actions cannot be used with bulk creates"
    end

    upsert? = opts[:upsert?] || action.upsert?
    upsert_fields = opts[:upsert_fields] || action.upsert_fields

    if upsert? && !upsert_fields do
      raise ArgumentError,
            "For bulk actions, `upsert_fields` must be specified if upsert? is set to true`"
    end

    {_, opts} = Ash.Actions.Helpers.add_process_context(api, Ash.Changeset.new(resource), opts)

    manual_action_can_bulk? =
      case action.manual do
        {mod, _opts} ->
          function_exported?(mod, :bulk_create, 3)

        _ ->
          false
      end

    data_layer_can_bulk? = Ash.DataLayer.data_layer_can?(resource, :bulk_create)

    batch_size =
      if data_layer_can_bulk? || manual_action_can_bulk? do
        opts[:batch_size] || 100
      else
        1
      end

    all_changes =
      action.changes
      |> Enum.concat(Ash.Resource.Info.changes(resource, action.type))
      |> Enum.with_index()

    result =
      inputs
      |> Stream.with_index()
      |> Stream.map(fn {input, index} ->
        resource
        |> Ash.Changeset.new()
        |> Map.put(:api, api)
        |> Ash.Changeset.set_context(%{
          private: %{
            upsert?: opts[:upsert?] || action.upsert? || false,
            upsert_identity: opts[:upsert_identity] || action.upsert_identity,
            upsert_fields: opts[:upsert_fields] || action.upsert_fields
          }
        })
        |> Ash.Actions.Helpers.add_context(opts)
        |> Ash.Changeset.set_context(%{bulk_create: %{index: index}})
        |> Ash.Changeset.prepare_changeset_for_action(action, opts, input)
        |> Ash.Changeset.run_before_transaction_hooks()
      end)
      |> transform_and_stop_on_errors(opts)
      |> Stream.transform(
        fn -> %{batch: [], count: 0, must_return_records?: opts[:notify?]} end,
        fn
          {:ok, item}, state when state.count < batch_size ->
            must_return_records? = state.must_return_records? || !Enum.empty?(item.after_action)

            {[],
             %{
               state
               | batch: [item | state.batch],
                 count: state.count + 1,
                 must_return_records?: must_return_records?
             }}

          {:ok, item}, state ->
            {[batch(state)], %{state | batch: [item], count: 0, must_return_records?: false}}

          {:error, error}, state ->
            if opts[:return_errors?] do
              {[{:error, error}], state}
            else
              {[], state}
            end
        end,
        fn state ->
          {[batch(state)], state}
        end,
        fn _ -> :ok end
      )
      |> Stream.map(fn
        {:error, error} ->
          {:error, error}

        {:batch, batch_config} ->
          %{count: count, batch: batch, must_return_records?: must_return_records?} = batch_config

          batch =
            Stream.map(batch, fn changeset ->
              Ash.Changeset.set_defaults(changeset, :create, true)
            end)

          if opts[:transaction] == :batch &&
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
                List.wrap(resource) ++ (action.touches_resources || []),
                fn ->
                  %{
                    must_return_records?: must_return_records_for_changes?,
                    batch: batch,
                    changes: changes
                  } =
                    batch
                    |> run_action_changes(
                      all_changes,
                      action,
                      opts[:actor],
                      opts[:authorize?],
                      opts[:tracer]
                    )

                  {batch, before_batch_notifications} =
                    batch
                    |> authorize(api, opts)
                    |> Enum.to_list()
                    |> run_bulk_before_batches(
                      changes,
                      all_changes,
                      opts
                    )

                  {batch, changesets_by_index} =
                    Enum.reduce(batch, {[], %{}}, fn changeset,
                                                     {changesets, changesets_by_index} ->
                      {[changeset | changesets],
                       Map.put(
                         changesets_by_index,
                         changeset.context.bulk_create.index,
                         changeset
                       )}
                    end)

                  run_batch(
                    resource,
                    batch,
                    action,
                    opts,
                    count,
                    must_return_records?,
                    must_return_records_for_changes?,
                    data_layer_can_bulk?,
                    api,
                    before_batch_notifications
                  )
                  |> run_after_action_hooks(changesets_by_index)
                  |> process_results(changes, all_changes, opts)
                end,
                opts[:timeout],
                %{
                  type: :bulk_create,
                  metadata: %{
                    resource: resource,
                    action: action.name,
                    actor: opts[:actor]
                  }
                }
              )
              |> case do
                {:ok, result} ->
                  result

                {:error, error} ->
                  {:error, error}
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
            %{
              must_return_records?: must_return_records_for_changes?,
              batch: batch,
              changes: changes
            } =
              batch
              |> run_action_changes(
                all_changes,
                action,
                opts[:actor],
                opts[:authorize?],
                opts[:tracer]
              )

            {batch, before_batch_notifications} =
              batch
              |> authorize(api, opts)
              |> Enum.to_list()
              |> run_bulk_before_batches(
                changes,
                all_changes,
                opts
              )

            {batch, changesets_by_index} =
              Enum.reduce(batch, {[], %{}}, fn changeset, {changesets, changesets_by_index} ->
                {[changeset | changesets],
                 Map.put(
                   changesets_by_index,
                   changeset.context.bulk_create.index,
                   changeset
                 )}
              end)

            run_batch(
              resource,
              batch,
              action,
              opts,
              count,
              must_return_records?,
              must_return_records_for_changes?,
              data_layer_can_bulk?,
              api,
              before_batch_notifications
            )
            |> run_after_action_hooks(changesets_by_index)
            |> process_results(changes, all_changes, opts)
          end
      end)

    if opts[:return_stream?] do
      Stream.transform(
        result,
        fn -> nil end,
        fn item, nil ->
          case item do
            {:error, error} ->
              if opts[:stop_on_error?] do
                {:halt, {[], error}}
              else
                {error
                 |> List.wrap()
                 |> Stream.map(&{:error, &1}), nil}
              end

            {:error, notifications, error} ->
              if opts[:stop_on_error?] do
                {:halt, {[], error}}
              else
                {error
                 |> List.wrap()
                 |> Stream.map(&{:error, &1})
                 |> notify_stream(notifications, resource, action, opts), nil}
              end

            {:ok, invalid, notifications} ->
              if opts[:stop_on_error?] && !Enum.empty?(invalid) do
                {:halt, {[], invalid}}
              else
                {invalid
                 |> Stream.map(&{:error, &1})
                 |> notify_stream(notifications, resource, action, opts), nil}
              end

            {:ok, batch_result, invalid, notifications} ->
              if opts[:stop_on_error?] && !Enum.empty?(invalid) do
                {:halt, {invalid, batch_result}}
              else
                if opts[:return_records?] do
                  {batch_result
                   |> Stream.map(&{:ok, &1})
                   |> Stream.concat(Stream.map(invalid, &{:error, &1}))
                   |> notify_stream(notifications, resource, action, opts), nil}
                else
                  {invalid
                   |> Stream.map(&{:error, &1})
                   |> notify_stream(notifications, resource, action, opts), nil}
                end
              end

            :ok ->
              {[], nil}
          end
        end,
        fn
          {errors, successes} ->
            remaining_items =
              successes
              |> Stream.map(&{:ok, &1})
              |> Stream.concat(Stream.map(errors, &{:error, &1}))

            {remaining_items, nil}

          nil ->
            {[], nil}
        end,
        & &1
      )
    else
      result
      |> Enum.reduce(%Ash.BulkResult{status: :empty, records: [], errors: []}, fn
        {:error, error}, result ->
          %{
            result
            | errors: [error | result.errors],
              status: errored(result.status)
          }

        {:error, notifications, error}, result ->
          %{
            result
            | errors: [error | result.errors],
              status: errored(result.status),
              notifications: (result.notifications || []) ++ notifications
          }

        {:ok, [], notifications}, result ->
          %{
            result
            | notifications: (result.notifications || []) ++ notifications,
              status: success(result.status)
          }

        {:ok, invalid, notifications}, result ->
          %{
            result
            | errors: invalid ++ result.errors,
              status: :partial_success,
              notifications: (result.notifications || []) ++ notifications
          }

        {:ok, batch_result, invalid, notifications}, result ->
          status =
            case invalid do
              [] -> success(result.status)
              _ -> :partial_success
            end

          records =
            if opts[:return_records?] do
              Enum.concat(Enum.to_list(batch_result), result.records)
            else
              result.records
            end

          %{
            result
            | records: records,
              errors: invalid ++ result.errors,
              status: status,
              notifications: (result.notifications || []) ++ notifications
          }

        :ok, result ->
          result
      end)
      |> case do
        %{status: :empty} = result ->
          %{result | status: :success}

        other ->
          other
      end
    end
  catch
    {:error, error, batch_number, notifications} ->
      status =
        if batch_number > 1 do
          :partial_success
        else
          :error
        end

      %Ash.BulkResult{
        status: status,
        errors: List.wrap(error),
        notifications: notifications
      }
  end

  defp run_bulk_before_batches(
         batch,
         changes,
         all_changes,
         opts
       ) do
    all_changes
    |> Enum.with_index()
    |> Enum.filter(fn
      {%{change: {module, _opts}}, _} ->
        function_exported?(module, :before_batch, 3)

      _ ->
        false
    end)
    |> Enum.reduce(batch, fn {%{change: {module, change_opts}}, index}, batch ->
      {matches, non_matches} =
        batch
        |> Enum.split_with(fn
          %{valid?: false} ->
            false

          changeset ->
            changes[index] == :all or
              changeset.context.bulk_create.index in List.wrap(changes[index])
        end)

      before_batch_results =
        module.before_batch(matches, change_opts, %{
          actor: opts[:actor],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        })

      Enum.concat([before_batch_results, non_matches])
    end)
    |> Enum.reduce(
      {[], []},
      fn
        %Ash.Notifier.Notification{} = notification, {changesets, notifications} ->
          {changesets, [notification | notifications]}

        result, {changesets, notifications} ->
          {[result | changesets], notifications}
      end
    )
  end

  defp notify_stream(stream, notifications, resource, action, opts) do
    if opts[:notify?] do
      notifications = List.wrap(notifications)

      if opts[:return_notifications?] do
        Stream.concat(stream, Stream.map(notifications, &{:notification, &1}))
      else
        remaining = Ash.Notifier.notify(notifications)

        Ash.Actions.Helpers.warn_missed!(resource, action, %{
          resource_notifications: remaining
        })

        stream
      end
    else
      stream
    end
  end

  defp errored(:empty), do: :error
  defp errored(:partial_success), do: :partial_success
  defp errored(:success), do: :partial_success
  defp errored(:error), do: :error

  defp success(:empty), do: :success
  defp success(:partial_success), do: :partial_success
  defp success(:error), do: :partial_success
  defp success(:success), do: :success

  defp transform_and_stop_on_errors(stream, opts) do
    Stream.map(stream, fn changeset ->
      if changeset.valid? do
        {:ok, changeset}
      else
        if opts[:stop_on_error?] && !opts[:return_stream?] do
          throw({:error, Ash.Error.to_error_class(changeset.error), 0, []})
        else
          changeset
        end
      end
    end)
  end

  defp authorize(batch, api, opts) do
    if opts[:authorize?] do
      batch
      |> Stream.map(fn changeset ->
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

  defp handle_bulk_result(%Ash.BulkResult{} = bulk_result, resource, action, opts) do
    bulk_result
    |> notify(resource, action, opts)
    |> sort(opts)
  end

  # for when we return a stream
  defp handle_bulk_result(stream, _, _, _), do: stream

  defp sort(%{records: records} = result, opts) when is_list(records) do
    if opts[:sorted?] do
      %{result | records: Enum.sort_by(records, & &1.__metadata__.bulk_create_index)}
    else
      result
    end
  end

  defp sort(result, _), do: result

  defp notify(%{notifications: []} = result, _resource, _action, _opts), do: result

  defp notify(%{notifications: notifications} = result, resource, action, opts) do
    if opts[:return_notifications?] do
      result
    else
      if opts[:notify?] do
        result = %{result | notifications: Ash.Notifier.notify(notifications)}

        Ash.Actions.Helpers.warn_missed!(resource, action, %{
          resource_notifications: result.notifications
        })

        result
      else
        result
      end
    end
  end

  defp run_batch(
         resource,
         batch,
         action,
         opts,
         count,
         must_return_records?,
         must_return_records_for_changes?,
         data_layer_can_bulk?,
         api,
         before_batch_notifications
       ) do
    {batch, invalid, notifications} =
      Enum.reduce(batch, {[], [], before_batch_notifications}, fn changeset,
                                                                  {changesets, invalid,
                                                                   notifications} ->
        if changeset.valid? do
          {changeset, %{notifications: new_notifications}} =
            Ash.Changeset.run_before_actions(changeset)

          if changeset.valid? do
            {[changeset | changesets], invalid, notifications ++ new_notifications}
          else
            if opts[:stop_on_error?] && !opts[:return_stream?] do
              throw({:error, Ash.Error.to_error_class(changeset.error), 0, []})
            end

            {changesets, [changeset | invalid], notifications ++ new_notifications}
          end
        else
          {changesets, [changeset | invalid], notifications}
        end
      end)

    case batch do
      [] ->
        {:ok, [], invalid, notifications}

      batch ->
        upsert_keys =
          if opts[:upsert?] do
            case opts[:upsert_identity] || action.upsert_identity do
              nil ->
                Ash.Resource.Info.primary_key(resource)

              identity ->
                keys =
                  resource
                  |> Ash.Resource.Info.identities()
                  |> Enum.find(&(&1.name == identity))
                  |> Kernel.||(
                    raise ArgumentError,
                          "No identity found for #{inspect(resource)} called #{inspect(identity)}"
                  )
                  |> Map.get(:keys)

                if opts[:tenant] &&
                     Ash.Resource.Info.multitenancy_strategy(resource) == :attribute do
                  [Ash.Resource.Info.multitenancy_attribute(resource) | keys]
                else
                  keys
                end
            end
          end

        case action.manual do
          {mod, opts} ->
            if function_exported?(mod, :bulk_create, 3) do
              mod.bulk_create(batch, opts, %{
                actor: opts[:actor],
                authorize?: opts[:authorize?],
                tracer: opts[:tracer],
                api: api,
                batch_size: count,
                upsert?: opts[:upsert?] || action.upsert?,
                upsert_keys: upsert_keys,
                upsert_fields: opts[:upsert_fields] || action.upsert_fields,
                return_records?:
                  opts[:return_records?] || must_return_records? ||
                    must_return_records_for_changes?,
                tenant: opts[:tenant]
              })
            else
              [changeset] = batch

              result =
                mod.create(changeset, opts, %{
                  actor: opts[:actor],
                  tenant: opts[:tenant],
                  authorize?: opts[:authorize?],
                  tracer: opts[:tracer],
                  api: api
                })

              case result do
                {:ok, result} ->
                  {:ok,
                   [
                     Ash.Resource.put_metadata(
                       result,
                       :bulk_create_index,
                       changeset.context.bulk_create.index
                     )
                   ]}

                {:error, error} ->
                  {:error, error}
              end
            end

          _ ->
            if data_layer_can_bulk? do
              Ash.DataLayer.bulk_create(resource, batch, %{
                batch_size: count,
                return_records?:
                  opts[:return_records?] || must_return_records? ||
                    must_return_records_for_changes?,
                upsert?: opts[:upsert?] || action.upsert? || false,
                upsert_keys: upsert_keys,
                upsert_fields: opts[:upsert_fields] || action.upsert_fields,
                tenant: opts[:tenant]
              })
            else
              [changeset] = batch

              case Ash.DataLayer.create(resource, changeset) do
                {:ok, result} ->
                  {:ok,
                   [
                     Ash.Resource.put_metadata(
                       result,
                       :bulk_create_index,
                       changeset.context.bulk_create.index
                     )
                   ]}

                {:error, error} ->
                  {:error, error}
              end
            end
        end
        |> case do
          {:ok, result} ->
            {:ok, result, invalid, notifications}

          :ok ->
            {:ok, invalid, notifications}

          other ->
            other
        end
    end
  end

  defp run_after_action_hooks({:ok, invalid, notifications}, changesets_by_index) do
    {:ok, invalid, notifications, changesets_by_index}
  end

  defp run_after_action_hooks({:ok, batch_results, invalid, notifications}, changesets_by_index) do
    batch_results
    |> Enum.reduce_while(
      {:ok, [], notifications, changesets_by_index},
      fn result, {:ok, records, notifications, changesets_by_index} ->
        changeset = changesets_by_index[result.__metadata__.bulk_create_index]

        case Ash.Changeset.run_after_actions(result, changeset, []) do
          {:error, error} ->
            {:halt, {:error, error}}

          {:ok, result, changeset, %{notifications: new_notifications}} ->
            {:cont,
             {:ok, [result | records], notifications ++ new_notifications,
              Map.put(changesets_by_index, result.__metadata__.bulk_create_index, changeset)}}
        end
      end
    )
    |> case do
      {:ok, results, notifications, changesets_by_index} ->
        {:ok, results, invalid, notifications, changesets_by_index}

      other ->
        other
    end
  end

  defp run_after_action_hooks({:error, error}, _) do
    {:error, error}
  end

  defp process_results({:ok, invalid, notifications, _changesets_by_index}, _, _, _),
    do: {:ok, invalid, notifications}

  defp process_results(
         {:ok, batch, invalid, notifications, changesets_by_index},
         changes,
         all_changes,
         opts
       ) do
    Enum.reduce(
      batch,
      {[], notifications, changesets_by_index, []},
      fn result, {results, notifications, changesets_by_index, errors} ->
        changeset = changesets_by_index[result.__metadata__.bulk_create_index]
        notifications = notifications ++ [notification(changeset, result, opts)]

        try do
          case Ash.Changeset.run_after_transactions({:ok, result}, changeset) do
            {:ok, result} ->
              {[result | results], notifications, changesets_by_index, errors}

            {:error, error} ->
              {results, notifications, changesets_by_index, [error | errors]}
          end
        rescue
          e ->
            {results, notifications, changesets_by_index, [e | errors]}
        end
      end
    )
    |> case do
      {results, notifications, changesets_by_index, []} ->
        case run_bulk_after_changes(changes, all_changes, results, changesets_by_index, opts) do
          {results, new_notifications, []} ->
            {:ok, results, invalid, new_notifications ++ notifications}

          {_results, _new_notifications, errors} ->
            {:error, notifications, Ash.Error.to_ash_error(errors)}
        end

      {_result, notifications, _, errors} ->
        {:error, notifications, Ash.Error.to_ash_error(errors)}
    end
  end

  defp process_results({:error, error}, _, _, _), do: {:error, error}

  defp run_bulk_after_changes(changes, all_changes, results, changesets_by_index, opts) do
    results =
      Stream.map(results, fn result ->
        {:ok, result}
      end)

    all_changes
    |> Enum.with_index()
    |> Enum.filter(fn
      {%{change: {module, _opts}}, _} ->
        function_exported?(module, :after_batch, 3)

      _ ->
        false
    end)
    |> Enum.reduce(results, fn {%{change: {module, change_opts}}, index}, results ->
      {matches, non_matches} =
        results
        |> Enum.split_with(fn
          {:ok, result} ->
            changes[index] == :all or
              result.__metadata__.bulk_create_index in List.wrap(changes[index])

          _ ->
            false
        end)

      matches =
        Enum.map(matches, fn match ->
          {changesets_by_index[match.__metadata__.bulk_create_index], match}
        end)

      after_batch_results =
        module.after_batch(matches, change_opts, %{
          actor: opts[:actor],
          tracer: opts[:tracer],
          authorize?: opts[:authorize?]
        })

      Enum.concat([after_batch_results, non_matches])
    end)
    |> Enum.reduce(
      {[], [], []},
      fn
        %Ash.Notifier.Notification{} = notification, {results, notifications, errors} ->
          {results, [notification | notifications], errors}

        {:ok, result}, {results, notifications, errors} ->
          {[result | results], notifications, errors}

        {:error, error}, {results, notifications, errors} ->
          {results, notifications, [error | errors]}
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

  defp run_action_changes(batch, all_changes, _action, actor, authorize?, tracer) do
    Enum.reduce(
      all_changes,
      %{must_return_records?: false, batch: batch, changes: %{}, notifications: []},
      fn
        {%{validation: {module, opts}} = validation, _change_index}, state ->
          batch =
            Stream.map(batch, fn changeset ->
              if Enum.all?(validation.where || [], fn {module, opts} ->
                   opts =
                     Ash.Filter.build_filter_from_template(
                       opts,
                       actor,
                       changeset.arguments,
                       changeset.context
                     )

                   module.validate(changeset, opts) == :ok
                 end) do
                module.validate(changeset, opts)
              else
                changeset
              end
            end)

          %{
            must_return_records?: state.must_return_records?,
            batch: batch,
            changes: state.changes
          }

        {%{change: {module, change_opts}} = change, change_index}, state ->
          if Enum.empty?(change.where) && !change.only_when_valid? do
            context = %{
              actor: actor,
              authorize?: authorize? || false,
              tracer: tracer
            }

            batch = batch_change(module, batch, change_opts, context, actor)

            must_return_records? =
              state.must_return_records? || function_exported?(module, :after_batch, 3)

            %{
              must_return_records?: must_return_records?,
              batch: Enum.to_list(batch),
              changes: Map.put(state.changes, change_index, :all)
            }
          else
            {matches, non_matches} =
              batch
              |> Enum.split_with(fn changeset ->
                applies_from_where? =
                  Enum.all?(change.where || [], fn {module, opts} ->
                    opts =
                      Ash.Filter.build_filter_from_template(
                        opts,
                        actor,
                        changeset.arguments,
                        changeset.context
                      )

                    module.validate(changeset, opts) == :ok
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
                must_return_records?: state.must_return_records?,
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
                state.must_return_records? || function_exported?(module, :after_batch, 3)

              %{
                must_return_records?: must_return_records?,
                batch: Enum.concat(matches, non_matches),
                changes:
                  Map.put(
                    state.changes,
                    change_index,
                    Enum.map(matches, & &1.context.bulk_create.index)
                  )
              }
            end
          end
      end
    )
  end

  defp batch_change(module, batch, change_opts, context, actor) do
    built_change_opts =
      Ash.Filter.build_filter_from_template(
        change_opts,
        actor,
        %{},
        context
      )

    # TODO: We should figure out how to remove this requirement
    # the basic problem is that if someone writes `set_attribute(:foo, ^arg(:arg))`
    # we can't use `.batch_change/3` (because which argument value do we pass in?)
    if function_exported?(module, :batch_change, 3) && built_change_opts == change_opts do
      module.batch_change(batch, change_opts, context)
    else
      Stream.map(batch, fn changeset ->
        change_opts =
          Ash.Filter.build_filter_from_template(
            change_opts,
            actor,
            changeset.arguments,
            changeset.context
          )

        module.change(changeset, change_opts, Map.put(context, :bulk?, true))
      end)
    end
  end

  defp batch(state) do
    {:batch,
     %{
       count: state.count,
       batch: state.batch,
       must_return_records?: state.must_return_records?
     }}
  end
end
