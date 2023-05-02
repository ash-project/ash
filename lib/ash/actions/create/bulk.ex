defmodule Ash.Actions.Create.Bulk do
  @moduledoc false
  @spec run(Ash.Api.t(), Ash.Resource.t(), atom(), Enumerable.t(map), Keyword.t()) ::
          :ok
          | {:ok, [Ash.Resource.record()]}
          | {:ok, [Ash.Resource.record()], [Ash.Notifier.Notification.t()]}
          | {:error, term}
  def run(api, resource, action, inputs, opts) do
    action = Ash.Resource.Info.action(resource, action)

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

    if opts[:upsert?] || action.upsert? do
      raise "Cannot upsert bulk actions currently"
    end

    # TODO: add process context without a changeset
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
        |> Ash.Actions.Helpers.add_context(opts)
        |> Ash.Changeset.set_context(%{bulk_create: %{index: index}})
        |> Ash.Changeset.prepare_changeset_for_action(action, opts, input)
        |> Ash.Changeset.run_before_transaction_hooks()
      end)
      |> Stream.map(fn changeset ->
        if changeset.valid? do
          {:ok, changeset}
        else
          if opts[:stop_on_error?] do
            throw({:error, Ash.Error.to_error_class(changeset.error), 0, []})
          else
            {:error, changeset}
          end
        end
      end)
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
                    api
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
              api
            )
            |> run_after_action_hooks(changesets_by_index)
            |> process_results(changes, all_changes, opts)
          end
      end)

    if opts[:return_stream?] do
      raise "can't do this yet"
    else
      result
      |> Enum.reduce(%Ash.BulkResult{status: :success, records: [], errors: []}, fn
        {:error, notifications, error}, result ->
          %{
            result
            | errors: [error | result.errors],
              status: :partial_success,
              notifications: notifications
          }

        {:ok, batch_result, notifications}, result ->
          notifications =
            if opts[:notify?] do
              Ash.Notifier.notify(notifications)
            else
              []
            end

          records = Enum.concat(Enum.to_list(batch_result), result.records)

          if opts[:return_notifications] do
            %{result | records: records, notifications: notifications}
          else
            notifications =
              if Process.get(:ash_started_transaction?) do
                current_notifications = Process.get(:ash_notifications, [])

                Process.put(
                  :ash_notifications,
                  current_notifications ++ notifications
                )

                []
              else
                notifications
              end

            %{result | records: records, notifications: notifications}
          end

        :ok, result ->
          result
      end)
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

  defp handle_bulk_result(bulk_result, resource, action, opts) do
    bulk_result
    |> notify(resource, action, opts)
    |> sort(opts)
  end

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
         api
       ) do
    {batch, notifications} =
      Enum.reduce(batch, {[], []}, fn changeset, {changesets, notifications} ->
        {changeset, %{notifications: new_notifications}} =
          Ash.Changeset.run_before_actions(changeset)

        {[changeset | changesets], notifications ++ new_notifications}
      end)

    case action.manual do
      {mod, opts} ->
        if function_exported?(mod, :bulk_create, 3) do
          mod.bulk_create(batch, opts, %{
            actor: opts[:actor],
            authorize?: opts[:authorize?],
            tracer: opts[:tracer],
            api: api,
            batch_size: count,
            return_records?:
              opts[:return_records?] || must_return_records? ||
                must_return_records_for_changes?,
            tenant: opts[:tenant]
          })
        else
          [changeset] = batch

          case mod.create(changeset, opts, %{
                 actor: opts[:actor],
                 tenant: opts[:tenant],
                 authorize?: opts[:authorize?],
                 tracer: opts[:tracer],
                 api: api
               }) do
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
        {:ok, result, notifications}

      other ->
        other
    end
  end

  defp run_after_action_hooks(:ok, _), do: :ok

  defp run_after_action_hooks({:ok, batch_results, notifications}, changesets_by_index) do
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
        {:ok, results, notifications, changesets_by_index}

      other ->
        other
    end
  end

  defp run_after_action_hooks({:error, error}, _) do
    {:error, error}
  end

  defp process_results(:ok, _, _, _), do: :ok

  defp process_results(
         {:ok, batch, notifications, changesets_by_index},
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
            {:ok, results, new_notifications ++ notifications}

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
    # TODO: support action.delay_global_validations?
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

        {%{change: {module, opts}} = change, change_index}, state ->
          if Enum.empty?(change.where) && !change.only_when_valid? do
            context = %{
              actor: actor,
              authorize?: authorize? || false,
              tracer: tracer
            }

            batch = module.batch_change(batch, opts, context)

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

              matches = module.batch_change(matches, opts, context)

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

  defp batch(state) do
    {:batch,
     %{
       count: state.count,
       batch: state.batch,
       must_return_records?: state.must_return_records?
     }}
  end
end
