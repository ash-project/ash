# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Create.Bulk do
  @moduledoc false
  @spec run(Ash.Domain.t(), Ash.Resource.t(), atom(), Enumerable.t(map), Keyword.t()) ::
          Ash.BulkResult.t()
  def run(domain, resource, action_name, inputs, opts) do
    action = Ash.Resource.Info.action(resource, action_name)

    opts =
      if opts[:return_notifications?] do
        Keyword.put(opts, :notify?, true)
      else
        opts
      end

    if !action do
      raise Ash.Error.Invalid.NoSuchAction, resource: resource, action: action_name, type: :create
    end

    if opts[:transaction] == :all && opts[:return_stream?] do
      raise ArgumentError,
            "Cannot specify `transaction: :all` and `return_stream?: true` together"
    end

    if opts[:return_stream?] && opts[:sorted?] do
      raise ArgumentError, "Cannot specify `sorted?: true` and `return_stream?: true` together"
    end

    opts =
      Keyword.put_new_lazy(opts, :select, fn ->
        resource |> Ash.Resource.Info.attributes() |> Enum.map(& &1.name)
      end)

    if opts[:transaction] == :all &&
         Ash.DataLayer.data_layer_can?(resource, :transact) do
      notify? = opts[:notify?] && !Process.put(:ash_started_transaction?, true)

      Ash.DataLayer.transaction(
        List.wrap(resource) ++ action.touches_resources,
        fn ->
          do_run(domain, resource, action, inputs, opts)
        end,
        opts[:timeout],
        %{
          type: :bulk_create,
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
      domain
      |> do_run(resource, action, inputs, opts)
      |> handle_bulk_result(resource, action, opts)
    end
  end

  def do_run(domain, resource, action, inputs, opts) do
    opts = Ash.Actions.Helpers.set_opts(opts, domain)

    upsert? = opts[:upsert?] || action.upsert?
    upsert_fields = opts[:upsert_fields] || action.upsert_fields

    if upsert? && !upsert_fields do
      raise ArgumentError,
            "For bulk actions, `upsert_fields` must be specified if upsert? is set to true`"
    end

    {_, opts} =
      Ash.Actions.Helpers.set_context_and_get_opts(
        domain,
        %{Ash.Changeset.new(resource) | action: action},
        opts
      )

    manual_action_can_bulk? =
      case action.manual do
        {mod, _opts} ->
          function_exported?(mod, :bulk_create, 3)

        _ ->
          false
      end

    base_changeset = base_changeset(resource, domain, opts, action)

    data_layer_can_bulk? =
      if Ash.DataLayer.data_layer_can?(resource, :bulk_create) do
        # If upserting with return_skipped_upsert? and data layer can't handle it in bulk, fall back to single inserts
        !upsert? || !base_changeset.context[:private][:return_skipped_upsert?] ||
          Ash.DataLayer.data_layer_can?(resource, :bulk_upsert_return_skipped)
      else
        false
      end

    batch_size =
      cond do
        action.manual != nil and manual_action_can_bulk? -> opts[:batch_size] || 100
        action.manual == nil and data_layer_can_bulk? -> opts[:batch_size] || 100
        true -> 1
      end

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

    belongs_to_attrs =
      resource
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(&(&1.type == :belongs_to))
      |> Enum.map(& &1.source_attribute)

    attrs_to_require =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.reject(&(&1.allow_nil? || &1.generated? || &1.name in belongs_to_attrs))

    action_select =
      if Ash.DataLayer.data_layer_can?(resource, :action_select) do
        Enum.uniq(
          Enum.concat(
            Ash.Resource.Info.action_select(
              resource,
              action
            ),
            List.wrap(
              opts[:select] ||
                MapSet.to_list(Ash.Resource.Info.selected_by_default_attribute_names(resource))
            )
          )
        )
      else
        MapSet.to_list(Ash.Resource.Info.attribute_names(resource))
      end

    ref = make_ref()

    changeset_stream =
      inputs
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
                lazy_matching_default_values(resource),
                base_changeset,
                argument_names
              )
            )
            |> handle_batch(
              domain,
              resource,
              action,
              all_changes,
              data_layer_can_bulk?,
              opts,
              ref,
              attrs_to_require,
              action_select
            )
          after
            if opts[:notify?] && !opts[:return_notifications?] do
              Process.put(
                {:bulk_notifications, ref},
                Ash.Notifier.notify(Process.delete({:bulk_notifications, ref}) || [])
              )
            end
          end
        end
      )

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

        {errors, error_count} = Process.get({:bulk_errors, ref}) || {[], 0}

        errors =
          if opts[:return_errors?] do
            Enum.map(errors, fn error ->
              Ash.Error.to_ash_error(error, [],
                bread_crumbs: [
                  "Exception raised in bulk create: #{inspect(resource)}.#{action.name}"
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

          %{records: [], error_count: _} ->
            %{bulk_result | status: :error}

          _ ->
            if Process.get({:any_success?, ref}) do
              %{bulk_result | status: :partial_success}
            else
              %{bulk_result | status: :error}
            end
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
            notifications: Process.delete({:bulk_notifications, ref})
          }

          {error_count, errors} = errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_errors, ref})
        Process.delete({:bulk_notifications, ref})
      end
    end
  rescue
    e ->
      reraise Ash.Error.to_error_class(e,
                stacktrace: __STACKTRACE__,
                bread_crumbs: [
                  "Exception raised in bulk create: #{inspect(resource)}.#{action.name}"
                ]
              ),
              __STACKTRACE__
  end

  defp pre_template_all_changes(action, resource, :create, base, actor, tenant) do
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
    if Ash.Expr.template_references_argument?(opts) ||
         Ash.Expr.template_references_context?(opts) do
      opts
    else
      {:templated,
       Ash.Expr.fill_template(
         opts,
         actor: actor,
         tenant: tenant,
         args: %{},
         context: changeset.context,
         changeset: changeset
       )}
    end
  end

  defp base_changeset(resource, domain, opts, action) do
    upsert_condition =
      case opts[:upsert_condition] do
        nil -> action && action.upsert_condition
        other -> other
      end

    resource
    |> Ash.Changeset.new()
    |> Map.put(:domain, domain)
    |> Map.put(:context, %{
      private: %{
        upsert?: opts[:upsert?] || action.upsert? || false,
        upsert_identity: opts[:upsert_identity] || action.upsert_identity,
        upsert_fields:
          Ash.Changeset.expand_upsert_fields(
            opts[:upsert_fields] || action.upsert_fields,
            resource
          ),
        upsert_condition: upsert_condition,
        return_skipped_upsert?:
          opts[:return_skipped_upsert?] || (action && action.return_skipped_upsert?) || false
      }
    })
    |> Ash.Actions.Helpers.add_context(opts)
    |> Ash.Changeset.set_context(opts[:context] || %{})
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
    |> Ash.Changeset.set_private_arguments_for_action(opts[:private_arguments] || %{})
    |> then(fn changeset ->
      if opts[:after_action] do
        Ash.Changeset.after_action(changeset, opts[:after_action])
      else
        changeset
      end
    end)
    |> then(fn
      changeset when upsert_condition != nil -> Ash.Changeset.filter(changeset, upsert_condition)
      changeset -> changeset
    end)
  end

  defp lazy_matching_default_values(resource) do
    resource
    |> Ash.Resource.Info.lazy_matching_default_attributes(:create)
    |> Enum.group_by(& &1.default)
    |> Enum.reduce(%{}, fn {fun, attributes}, lazy_matching_default_values ->
      default =
        case fun do
          {m, f, a} -> apply(m, f, a)
          fun -> fun.()
        end

      Enum.reduce(attributes, lazy_matching_default_values, fn attribute,
                                                               lazy_matching_default_values ->
        Map.put(lazy_matching_default_values, attribute.name, default)
      end)
    end)
  end

  defp error_stream(ref) do
    Stream.resource(
      fn -> Process.delete({:bulk_errors, ref}) end,
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

  defp handle_batch(
         batch,
         domain,
         resource,
         action,
         all_changes,
         data_layer_can_bulk?,
         opts,
         ref,
         attrs_to_require,
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
        opts[:actor],
        opts[:authorize?],
        opts[:tracer],
        opts[:tenant]
      )

    batch =
      if re_sort? do
        Enum.sort_by(batch, & &1.context.bulk_create.index)
      else
        batch
      end

    {batch, must_be_simple_results} =
      batch
      |> Enum.map(fn changeset ->
        Ash.Changeset.require_values(
          changeset,
          :create
        )
        |> Ash.Changeset.require_values(
          :update,
          false,
          changeset.action.require_attributes
        )
      end)
      |> authorize(opts)
      |> Ash.Actions.Helpers.split_and_run_simple(
        action,
        opts,
        changes,
        all_changes,
        :bulk_create,
        fn changeset ->
          case Ash.Actions.Create.run(domain, changeset, action, opts) do
            {:ok, result} ->
              Process.put({:any_success?, ref}, true)

              [
                Ash.Resource.set_metadata(result, %{
                  bulk_create_index: changeset.context.bulk_create.index
                })
              ]

            {:ok, result, notifications} ->
              Process.put({:any_success?, ref}, true)

              store_notification(ref, notifications, opts)

              [
                Ash.Resource.set_metadata(result, %{
                  bulk_create_index: changeset.context.bulk_create.index
                })
              ]

            {:error, error} ->
              store_error(ref, error, opts)
              []
          end
        end
      )

    batch =
      Enum.reject(batch, fn
        %{valid?: false} = changeset ->
          store_error(ref, changeset, opts)
          true

        _changeset ->
          false
      end)

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
              data_layer_can_bulk?,
              ref,
              changes,
              must_return_records_for_changes?,
              must_be_simple_results,
              attrs_to_require,
              action_select
            )
          end,
          opts[:timeout],
          %{
            type: :bulk_create,
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
          Process.delete(:ash_notifications)
          Process.delete(:ash_started_transaction?)

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
        data_layer_can_bulk?,
        ref,
        changes,
        must_return_records_for_changes?,
        must_be_simple_results,
        attrs_to_require,
        action_select
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
         data_layer_can_bulk?,
         ref,
         changes,
         must_return_records_for_changes?,
         must_be_simple_results,
         attrs_to_require,
         action_select
       ) do
    must_return_records? =
      opts[:notify?] ||
        Enum.any?(batch, fn item ->
          item.after_action != []
        end)

    batch =
      Ash.Actions.Update.Bulk.run_bulk_before_batches(
        batch,
        changes,
        all_changes,
        opts,
        ref,
        :bulk_create
      )

    {changesets_by_ref, changesets_by_index} = index_changesets(batch)

    run_batch(
      resource,
      batch,
      action,
      opts,
      must_return_records?,
      must_return_records_for_changes?,
      data_layer_can_bulk?,
      domain,
      ref,
      attrs_to_require,
      action_select,
      changesets_by_ref,
      changesets_by_index
    )
    |> run_after_action_hooks(opts, domain, ref)
    |> process_results(
      changes,
      all_changes,
      opts,
      ref,
      changesets_by_ref,
      changesets_by_index,
      batch,
      domain,
      resource,
      must_return_records_for_changes?,
      action
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
         {input, index},
         action,
         opts,
         lazy_matching_default_values,
         base,
         argument_names
       ) do
    base
    |> Ash.Changeset.put_context(:bulk_create, %{index: index, ref: make_ref()})
    |> Ash.Changeset.set_private_arguments_for_action(opts[:private_arguments] || %{})
    |> handle_params(
      Keyword.get(opts, :assume_casted?, false),
      action,
      opts,
      input,
      argument_names
    )
    |> set_lazy_non_matching_defaults()
    |> set_lazy_matching_defaults(lazy_matching_default_values)
    |> set_tenant()
  end

  defp set_tenant(changeset) do
    if changeset.tenant &&
         Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.Info.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.to_tenant | a])

      Ash.Changeset.force_change_attribute(changeset, attribute, attribute_value)
    else
      if is_nil(Ash.Resource.Info.multitenancy_strategy(changeset.resource)) ||
           Ash.Resource.Info.multitenancy_global?(changeset.resource) || changeset.tenant do
        changeset
      else
        Ash.Changeset.add_error(
          changeset,
          Ash.Error.Invalid.TenantRequired.exception(resource: changeset.resource)
        )
      end
    end
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

          try do
            Process.put(:ash_started_transaction?, true)
            batch_result = callback.(batch)
            {errors, _} = Process.get({:bulk_errors, ref}) || {[], 0}

            notifications =
              if opts[:notify?] do
                process_notifications = Process.delete(:ash_notifications) || []
                bulk_notifications = Process.delete({:bulk_notifications, ref}) || []

                if opts[:return_notifications?] do
                  process_notifications ++ bulk_notifications
                else
                  if opts[:transaction] && opts[:transaction] != :all do
                    Ash.Notifier.notify(bulk_notifications) ++
                      Ash.Notifier.notify(process_notifications)
                  else
                    []
                  end
                end
              end

            {batch_result, notifications, errors, Process.get({:any_success?, ref})}
          catch
            value ->
              {:throw, value}
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

  defp set_lazy_non_matching_defaults(changeset) do
    changeset.resource
    |> Ash.Resource.Info.lazy_non_matching_default_attributes(:create)
    |> Enum.reduce(changeset, fn attribute, changeset ->
      if Ash.Changeset.changing_attribute?(changeset, attribute.name) do
        changeset
      else
        Ash.Changeset.force_change_attribute(
          changeset,
          attribute.name,
          default(attribute)
        )
      end
    end)
  end

  defp set_lazy_matching_defaults(changeset, values) do
    Enum.reduce(values, changeset, fn {key, value}, changeset ->
      if Ash.Changeset.changing_attribute?(changeset, key) do
        changeset
      else
        Ash.Changeset.force_change_attribute(
          changeset,
          key,
          value
        )
      end
    end)
  end

  defp index_changesets(batch) do
    Enum.reduce(batch, {%{}, %{}}, fn changeset, {by_ref, by_index} ->
      ref = changeset.context.bulk_create.ref
      index = changeset.context.bulk_create.index

      {
        Map.put(by_ref, ref, changeset),
        Map.put(by_index, index, ref)
      }
    end)
  end

  defp default(%{default: {mod, func, args}}), do: apply(mod, func, args)
  defp default(%{default: function}) when is_function(function, 0), do: function.()
  defp default(%{default: value}), do: value

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
      {result.error_count + 1, [error | result.errors || []]}
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

  defp store_error(ref, empty, opts, error_count \\ nil)
  defp store_error(_ref, empty, _opts, _error_count) when empty in [[], nil], do: :ok

  defp store_error(ref, error, opts, error_count) do
    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error), 0})
    else
      if opts[:return_errors?] do
        {errors, count} = Process.get({:bulk_errors, ref}) || {[], 0}

        new_errors =
          error
          |> List.wrap()
          |> Enum.map(&Ash.Error.to_ash_error/1)

        Process.put(
          {:bulk_errors, ref},
          {new_errors ++ errors, count + (error_count || Enum.count(new_errors))}
        )
      else
        {errors, count} = Process.get({:bulk_errors, ref}) || {[], 0}
        Process.put({:bulk_errors, ref}, {errors, count + (error_count || 1)})
      end
    end
  end

  defp store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_notification(ref, notification, opts) do
    if opts[:notify?] || opts[:return_notifications?] do
      notifications = Process.get({:bulk_notifications, ref}) || []

      new_notifications =
        if is_list(notification) do
          notification ++ notifications
        else
          [notification | notifications]
        end

      Process.put({:bulk_notifications, ref}, new_notifications)
    end
  end

  defp authorize(batch, opts) do
    if opts[:authorize?] do
      Enum.map(batch, fn changeset ->
        if changeset.valid? do
          case Ash.can(changeset, opts[:actor],
                 return_forbidden_error?: true,
                 run_queries?: false,
                 pre_flight?: false,
                 maybe_is: false,
                 alter_source?: true
               ) do
            {:ok, true} ->
              changeset

            {:ok, true, changeset} ->
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
      %{result | records: Enum.sort_by(records, & &1.__metadata__.bulk_create_index)}
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
         data_layer_can_bulk?,
         domain,
         ref,
         attrs_to_require,
         action_select,
         changesets_by_ref,
         changesets_by_index
       ) do
    batch
    |> Enum.map(fn changeset ->
      with %{valid?: true} = changeset <- changeset,
           {changeset, %{notifications: new_notifications}} <-
             Ash.Changeset.run_before_actions(changeset),
           %{valid?: true} = changeset <-
             Ash.Changeset.require_values(
               changeset,
               :create,
               true,
               attrs_to_require
             ) do
        changeset =
          changeset
          |> Ash.Changeset.hydrate_atomic_refs(opts[:actor])
          |> Ash.Changeset.apply_atomic_constraints(opts[:actor])

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
                {changeset, _} =
                  Ash.Actions.ManagedRelationships.validate_required_belongs_to({changeset, []})

                {changeset, manage_instructions.notifications}
            end
          else
            {changeset, []}
          end

        store_notification(ref, manage_notifications, opts)

        changeset
      else
        {%Ash.Changeset{} = changeset, _} -> changeset
        %Ash.Changeset{} = changeset -> changeset
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
        {[], changesets_by_ref, changesets_by_index}

      batch ->
        upsert_keys =
          if opts[:upsert?] || action.upsert? do
            case opts[:upsert_identity] || action.upsert_identity do
              nil ->
                Ash.Resource.Info.primary_key(resource)

              identity ->
                keys =
                  resource
                  |> Ash.Resource.Info.identities()
                  |> Enum.find(&(&1.name == identity))
                  |> Kernel.||(
                    raise Ash.Error.Invalid.NoIdentityFound,
                      resource: resource,
                      identity: identity
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

        {changesets_by_ref, changesets_by_index} = index_changesets(batch)

        batch
        |> Enum.group_by(&{&1.atomics, &1.filter})
        |> Enum.flat_map(fn {_atomics, batch} ->
          result =
            case action.manual do
              {mod, manual_opts} ->
                source_context =
                  case batch do
                    [cs | _] ->
                      cs.context

                    _ ->
                      %{}
                  end

                if function_exported?(mod, :bulk_create, 3) do
                  mod.bulk_create(batch, manual_opts, %Ash.Resource.ManualCreate.BulkContext{
                    actor: opts[:actor],
                    select: opts[:select],
                    batch_size: opts[:batch_size],
                    authorize?: opts[:authorize?],
                    source_context: source_context,
                    tracer: opts[:tracer],
                    domain: domain,
                    upsert?: opts[:upsert?] || action.upsert?,
                    upsert_keys: upsert_keys,
                    identity:
                      (opts[:upsert_identity] || action.upsert_identity) &&
                        Ash.Resource.Info.identity(
                          resource,
                          opts[:upsert_identity] || action.upsert_identity
                        ),
                    upsert_fields:
                      Ash.Changeset.expand_upsert_fields(
                        opts[:upsert_fields] || action.upsert_fields,
                        resource
                      ),
                    return_records?:
                      opts[:return_records?] || must_return_records? ||
                        must_return_records_for_changes?,
                    return_notifications?: opts[:return_notifications?] || false,
                    return_errors?: opts[:return_errors?] || false,
                    tenant: opts[:tenant]
                  })
                else
                  ctx = %Ash.Resource.ManualCreate.Context{
                    actor: opts[:actor],
                    select: opts[:select],
                    authorize?: opts[:authorize?],
                    source_context: source_context,
                    tracer: opts[:tracer],
                    domain: domain,
                    upsert?: opts[:upsert?] || action.upsert?,
                    upsert_keys: upsert_keys,
                    identity:
                      (opts[:upsert_identity] || action.upsert_identity) &&
                        Ash.Resource.Info.identity(
                          resource,
                          opts[:upsert_identity] || action.upsert_identity
                        ),
                    upsert_fields:
                      Ash.Changeset.expand_upsert_fields(
                        opts[:upsert_fields] || action.upsert_fields,
                        resource
                      ),
                    return_notifications?: opts[:return_notifications?] || false,
                    tenant: opts[:tenant]
                  }

                  [changeset] = batch

                  [
                    mod.create(changeset, manual_opts, ctx)
                    |> Ash.Actions.BulkManualActionHelpers.process_non_bulk_result(
                      changeset,
                      :bulk_create
                    )
                  ]
                end
                |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                |> Ash.Actions.BulkManualActionHelpers.process_bulk_results(
                  mod,
                  :bulk_create,
                  &store_notification/3,
                  &store_error/3,
                  ref,
                  opts
                )

              _ ->
                if data_layer_can_bulk? do
                  Ash.DataLayer.bulk_create(
                    resource,
                    Stream.map(batch, fn changeset ->
                      %{changeset | tenant: changeset.to_tenant}
                    end),
                    %{
                      select: opts[:select],
                      batch_size: opts[:batch_size],
                      action_select: action_select,
                      identity:
                        (opts[:upsert_identity] || action.upsert_identity) &&
                          Ash.Resource.Info.identity(
                            resource,
                            opts[:upsert_identity] || action.upsert_identity
                          ),
                      return_records?:
                        opts[:return_records?] || must_return_records? ||
                          must_return_records_for_changes?,
                      upsert?: opts[:upsert?] || action.upsert? || false,
                      upsert_keys: upsert_keys,
                      upsert_fields:
                        Ash.Changeset.expand_upsert_fields(
                          opts[:upsert_fields] || action.upsert_fields,
                          resource
                        ),
                      upsert_condition:
                        case opts[:upsert_condition] do
                          nil -> action && action.upsert_condition
                          other -> other
                        end,
                      return_skipped_upsert?:
                        case opts[:return_skipped_upsert?] do
                          nil -> action && action.return_skipped_upsert?
                          other -> other
                        end,
                      tenant: Ash.ToTenant.to_tenant(opts[:tenant], resource)
                    }
                  )
                  |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                else
                  [changeset] = batch
                  upsert? = opts[:upsert?] || action.upsert? || false

                  result =
                    if upsert? do
                      Ash.DataLayer.upsert(
                        resource,
                        %{changeset | action_select: action_select},
                        upsert_keys
                      )
                    else
                      Ash.DataLayer.create(resource, %{changeset | action_select: action_select})
                    end

                  case result do
                    {:ok, {:upsert_skipped, _query, callback}} ->
                      if changeset.context[:private][:return_skipped_upsert?] do
                        case callback.() do
                          {:ok, record} ->
                            {:ok,
                             [
                               Ash.Resource.set_metadata(record, %{
                                 bulk_create_index: changeset.context.bulk_create.index
                               })
                             ]}

                          _ ->
                            []
                        end
                      else
                        []
                      end

                    {:ok, %{__metadata__: %{upsert_skipped: true}} = record} ->
                      if changeset.context[:private][:return_skipped_upsert?] do
                        {:ok,
                         [
                           Ash.Resource.set_metadata(record, %{
                             bulk_create_index: changeset.context.bulk_create.index
                           })
                         ]}
                      else
                        []
                      end

                    {:ok, result} ->
                      {:ok,
                       [
                         Ash.Resource.set_metadata(
                           result,
                           %{
                             bulk_create_index: changeset.context.bulk_create.index
                           }
                         )
                       ]}

                    {:error, :no_rollback, error} ->
                      {:error, :no_rollback, error}

                    {:error, error} ->
                      {:error, error}
                  end
                end
            end

          case result do
            {:ok, result} ->
              result =
                if tenant = opts[:tenant] do
                  Enum.map(result, fn record ->
                    %{record | __metadata__: Map.put(record.__metadata__, :tenant, tenant)}
                  end)
                else
                  result
                end

              Process.put({:any_success?, ref}, true)
              Ash.Actions.Helpers.select(result, %{resource: resource, select: action_select})

            :ok ->
              Process.put({:any_success?, ref}, true)
              []

            {:error, :no_rollback, error} ->
              store_error(ref, error, opts)

            {:error, error} ->
              store_error(ref, error, opts)
              []
          end
        end)
        |> then(&{&1, changesets_by_ref, changesets_by_index})
    end
  end

  defp manage_relationships(created, domain, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(domain, created, changeset, engine_opts),
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
         {batch_results, changesets_by_ref, changesets_by_index},
         opts,
         domain,
         ref
       ) do
    Enum.flat_map(batch_results, fn result ->
      changeset =
        Ash.Actions.Helpers.lookup_changeset(
          result,
          changesets_by_ref,
          changesets_by_index,
          index_key: :bulk_create_index,
          ref_key: :bulk_action_ref
        )

      case manage_relationships(result, domain, changeset,
             upsert?: opts[:upsert?],
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
         changesets_by_ref,
         changesets_by_index,
         changesets,
         domain,
         resource,
         must_return_records_for_changes?,
         action
       ) do
    results =
      Enum.flat_map(batch, fn result ->
        changeset =
          Ash.Actions.Helpers.lookup_changeset(
            result,
            changesets_by_ref,
            changesets_by_index,
            index_key: :bulk_create_index,
            ref_key: :bulk_action_ref
          )

        if opts[:notify?] || opts[:return_notifications?] do
          store_notification(ref, notification(changeset, result, opts), opts)
        end

        try do
          case Ash.Changeset.run_after_transactions(
                 {:ok, result},
                 changeset
               ) do
            {:ok, result} ->
              if opts[:return_records?] || must_return_records_for_changes? do
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

    changes
    |> Ash.Actions.Update.Bulk.run_bulk_after_changes(
      all_changes,
      results,
      changesets_by_ref,
      changesets_by_index,
      changesets,
      opts,
      ref,
      resource,
      :bulk_create_index,
      :bulk_action_ref
    )
    |> then(fn records ->
      if opts[:return_records?] do
        select =
          if opts[:select] do
            List.wrap(opts[:select])
          else
            resource |> Ash.Resource.Info.public_attributes() |> Enum.map(& &1.name)
          end

        case Ash.load(
               records,
               select,
               context: %{private: %{just_created_by_action: action.name}},
               reuse_values?: true,
               domain: domain,
               action: Ash.Resource.Info.primary_action(resource, :read) || action,
               tenant: opts[:tenant],
               actor: opts[:actor],
               authorize?: opts[:authorize?],
               tracer: opts[:tracer]
             ) do
          {:ok, records} ->
            Ash.load(
              records,
              List.wrap(opts[:load]),
              context: %{private: %{just_created_by_action: action.name}},
              domain: domain,
              tenant: opts[:tenant],
              action: Ash.Resource.Info.primary_action(resource, :read) || action,
              reuse_values?: true,
              actor: opts[:actor],
              authorize?: opts[:authorize?],
              tracer: opts[:tracer]
            )
            |> case do
              {:ok, records} ->
                Enum.reject(records, & &1.__metadata__[:private][:missing_from_data_layer])

              {:error, error} ->
                store_error(ref, error, opts)
                []
            end

          {:error, error} ->
            store_error(ref, error, opts)
            []
        end
      else
        []
      end
    end)
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

  defp run_action_changes(batch, all_changes, _action, actor, authorize?, tracer, tenant) do
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
        {%{validation: {module, opts}} = validation, _change_index}, %{batch: batch} = state ->
          batch =
            Enum.map(batch, fn changeset ->
              cond do
                !module.has_validate?() ->
                  Ash.Changeset.add_error(
                    changeset,
                    Ash.Error.Framework.CanNotBeAtomic.exception(
                      resource: changeset.resource,
                      change: module,
                      reason: "Create actions cannot be made atomic"
                    )
                  )

                invalid =
                    Enum.find(validation.where, fn {module, _} -> !module.has_validate?() end) ->
                  {module, _} = invalid

                  Ash.Changeset.add_error(
                    changeset,
                    Ash.Error.Framework.CanNotBeAtomic.exception(
                      resource: changeset.resource,
                      change: module,
                      reason: "Create actions cannot be made atomic"
                    )
                  )

                validation.only_when_valid? && !changeset.valid? ->
                  changeset

                Enum.all?(validation.where || [], fn {module, opts} ->
                  opts =
                      templated_opts(
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
                    struct(Ash.Resource.Validation.Context, context)
                  ) == :ok
                end) ->
                  opts =
                    templated_opts(
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
                         struct(
                           Ash.Resource.Validation.Context,
                           Map.put(context, :message, validation.message)
                         )
                       ) do
                    :ok ->
                      changeset

                    {:error, error} ->
                      error = Ash.Error.to_ash_error(error)

                      if validation.message do
                        error =
                          Ash.Error.override_validation_message(error, validation.message)

                        Ash.Changeset.add_error(changeset, error)
                      else
                        Ash.Changeset.add_error(changeset, error)
                      end
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
                (module.has_batch_change?() &&
                   module.has_after_batch?() &&
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
                    opts =
                      templated_opts(
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
                  (module.has_batch_change?() &&
                     module.has_after_batch?() &&
                     module.batch_callbacks?(batch, change_opts, context))

              %{
                state
                | must_return_records?: must_return_records?,
                  batch: Enum.concat(matches, non_matches),
                  re_sort?: true,
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
    case change_opts do
      {:templated, change_opts} ->
        if module.has_batch_change?() do
          module.batch_change(
            batch,
            change_opts,
            struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
          )
        else
          Enum.map(batch, fn changeset ->
            {:ok, change_opts} = module.init(change_opts)

            Ash.Resource.Change.change(
              module,
              changeset,
              change_opts,
              struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
            )
          end)
        end

      change_opts ->
        if module.has_batch_change?() do
          batch
          |> Enum.map(fn changeset ->
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

            module.batch_change(
              [changeset],
              change_opts,
              struct(struct(Ash.Resource.Change.Context, context), bulk?: true)
            )
          end)
          |> Enum.concat()
        else
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
        end
    end
  end
end
