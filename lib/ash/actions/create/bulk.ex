defmodule Ash.Actions.Create.Bulk do
  @moduledoc false
  @spec run(Ash.Domain.t(), Ash.Resource.t(), atom(), Enumerable.t(map), Keyword.t()) ::
          :ok
          | {:ok, [Ash.Resource.record()]}
          | {:ok, [Ash.Resource.record()], [Ash.Notifier.Notification.t()]}
          | {:error, term}
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
      Ash.Actions.Helpers.set_context_and_get_opts(domain, Ash.Changeset.new(resource), opts)

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

    ref = make_ref()

    lazy_matching_default_values = lazy_matching_default_values(resource)
    base_changeset = base_changeset(resource, domain, opts, action)

    all_changes =
      pre_template_all_changes(action, resource, action.type, base_changeset, opts[:actor])

    argument_names = Enum.map(action.arguments, & &1.name)

    belongs_to_attrs =
      resource
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(&(&1.type == :belongs_to))
      |> Enum.map(& &1.source_attribute)

    required_attrs_list =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.reject(&(&1.allow_nil? || &1.generated? || &1.name in belongs_to_attrs))

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
                lazy_matching_default_values,
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
              required_attrs_list
            )
          after
            if opts[:notify?] && !opts[:return_notifications?] do
              Ash.Notifier.notify(Process.delete({:bulk_create_notifications, ref}))
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
            []
          end

        notifications =
          if opts[:notify?] && opts[:return_notifications?] do
            Process.delete({:bulk_create_notifications, ref})
          else
            if opts[:notify?] do
              Ash.Notifier.notify(Process.delete({:bulk_create_notifications, ref}))
            else
              []
            end
          end

        {errors, error_count} = Process.get({:bulk_create_errors, ref}) || {[], 0}

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
            notifications: Process.delete({:bulk_create_notifications, ref})
          }

          {error_count, errors} = errors(result, error, opts)

          %{result | errors: errors, error_count: error_count}
      after
        Process.delete({:bulk_create_errors, ref})
        Process.delete({:bulk_create_notifications, ref})
      end
    end
  end

  defp pre_template_all_changes(action, resource, :create, base, actor) do
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
    if Ash.Expr.template_references_argument?(opts) ||
         Ash.Expr.template_references_context?(opts) do
      opts
    else
      {:templated,
       Ash.Expr.fill_template(
         opts,
         actor,
         %{},
         changeset.context
       )}
    end
  end

  defp base_changeset(resource, domain, opts, action) do
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
          )
      }
    })
    |> Ash.Actions.Helpers.add_context(opts)
    |> Ash.Changeset.set_context(opts[:context] || %{})
    |> Ash.Changeset.prepare_changeset_for_action(action, opts)
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
      fn -> Process.delete({:bulk_create_errors, ref}) end,
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
      fn -> Process.delete({:bulk_create_notifications, ref}) end,
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
         required_attrs_list
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
        opts[:tenant]
      )

    {batch, must_be_simple} =
      batch
      |> Stream.map(fn changeset ->
        Ash.Changeset.require_values(
          changeset,
          :create,
          true,
          required_attrs_list
        )
      end)
      |> Enum.reduce({[], []}, fn changeset, {batch, must_be_simple} ->
        if changeset.after_transaction in [[], nil] do
          changeset = Ash.Changeset.run_before_transaction_hooks(changeset)
          {[changeset | batch], must_be_simple}
        else
          {batch, [%{changeset | __validated_for_action__: action.name} | must_be_simple]}
        end
      end)

    must_be_simple_results =
      Enum.flat_map(must_be_simple, fn changeset ->
        case Ash.Actions.Create.run(domain, changeset, action, opts) do
          {:ok, result} ->
            [
              Ash.Resource.set_metadata(result, %{
                bulk_create_index: changeset.context.bulk_create.index
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
                data_layer_can_bulk?,
                ref,
                changes,
                must_return_records_for_changes?,
                must_be_simple_results
              )

            {new_errors, new_error_count} =
              Process.delete({:bulk_create_errors, tmp_ref}) || {[], 0}

            store_error(ref, new_errors, new_error_count)

            result
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
        data_layer_can_bulk?,
        ref,
        changes,
        must_return_records_for_changes?,
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
         data_layer_can_bulk?,
         ref,
         changes,
         must_return_records_for_changes?,
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
      data_layer_can_bulk?,
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
      resource
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
    |> Ash.Changeset.put_context(:bulk_create, %{index: index})
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
      changeset
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
      Task.async_stream(
        stream,
        fn batch ->
          try do
            Process.put(:ash_started_transaction?, true)
            batch_result = callback.(batch)
            {errors, _} = Process.get({:bulk_create_errors, ref}) || {[], 0}

            notifications =
              if opts[:notify?] do
                process_notifications = Process.get(:ash_notifications, [])
                bulk_notifications = Process.get({:bulk_create_notifications, ref}) || []

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

            {batch_result, notifications, errors}
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
    Enum.reduce(batch, %{}, fn changeset, changesets_by_index ->
      Map.put(
        changesets_by_index,
        changeset.context.bulk_create.index,
        changeset
      )
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
        function_exported?(module, :batch_change, 3) &&
          function_exported?(module, :before_batch, 3)

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
              changeset.context.bulk_create.index in List.wrap(changes[index])
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

  defp store_error(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_error(ref, error, opts) do
    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error), 0})
    else
      if opts[:return_errors?] do
        {errors, count} = Process.get({:bulk_create_errors, ref}) || {[], 0}

        new_errors =
          error
          |> List.wrap()
          |> Enum.map(&Ash.Error.to_ash_error/1)

        Process.put(
          {:bulk_create_errors, ref},
          {new_errors ++ errors, count + Enum.count(new_errors)}
        )
      else
        {errors, count} = Process.get({:bulk_create_errors, ref}) || {[], 0}
        Process.put({:bulk_create_errors, ref}, {errors, count + 1})
      end
    end
  end

  defp store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  defp store_notification(ref, notification, opts) do
    if opts[:notify?] || opts[:return_notifications?] do
      notifications = Process.get({:bulk_create_notifications, ref}) || []

      new_notifications =
        if is_list(notification) do
          notification ++ notifications
        else
          [notification | notifications]
        end

      Process.put({:bulk_create_notifications, ref}, new_notifications)
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
         ref
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

        batch
        |> Enum.group_by(&{&1.atomics, &1.filter})
        |> Enum.flat_map(fn {_atomics, batch} ->
          result =
            case action.manual do
              {mod, opts} ->
                if function_exported?(mod, :bulk_create, 3) do
                  mod.bulk_create(batch, opts, %Ash.Resource.ManualCreate.Context{
                    actor: opts[:actor],
                    select: opts[:select],
                    batch_size: opts[:batch_size],
                    authorize?: opts[:authorize?],
                    tracer: opts[:tracer],
                    domain: domain,
                    upsert?: opts[:upsert?] || action.upsert?,
                    upsert_keys: upsert_keys,
                    upsert_fields:
                      Ash.Changeset.expand_upsert_fields(
                        opts[:upsert_fields] || action.upsert_fields,
                        resource
                      ),
                    return_records?:
                      opts[:return_records?] || must_return_records? ||
                        must_return_records_for_changes?,
                    tenant: opts[:tenant]
                  })
                  |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                  |> case do
                    {:ok, results} ->
                      results

                    :ok ->
                      if opts[:return_records?] do
                        raise "`#{inspect(mod)}.bulk_create/3` returned :ok without a result when `return_records?` is true"
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
                    mod.create(changeset, opts, %Ash.Resource.ManualCreate.Context{
                      select: opts[:select],
                      actor: opts[:actor],
                      tenant: opts[:tenant],
                      authorize?: opts[:authorize?],
                      tracer: opts[:tracer],
                      domain: domain
                    })
                    |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)

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
                  Ash.DataLayer.bulk_create(
                    resource,
                    Stream.map(batch, fn changeset ->
                      %{changeset | tenant: changeset.to_tenant}
                    end),
                    %{
                      select: opts[:select],
                      batch_size: opts[:batch_size],
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
                      tenant: Ash.ToTenant.to_tenant(opts[:tenant], resource)
                    }
                  )
                  |> Ash.Actions.Helpers.rollback_if_in_transaction(resource, nil)
                else
                  [changeset] = batch
                  upsert? = opts[:upsert?] || action.upsert? || false

                  result =
                    if upsert? do
                      Ash.DataLayer.upsert(resource, changeset, upsert_keys)
                    else
                      Ash.DataLayer.create(resource, changeset)
                    end

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

                    {:error, :no_rollback, error} ->
                      {:error, :no_rollback, error}

                    {:error, error} ->
                      {:error, error}
                  end
                end
            end

          case result do
            {:ok, result} ->
              result

            :ok ->
              []

            {:error, :no_rollback, error} ->
              store_error(ref, error, opts)

            {:error, error} ->
              store_error(ref, error, opts)
              []
          end
        end)
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
         batch_results,
         opts,
         domain,
         ref,
         changesets_by_index
       ) do
    Enum.flat_map(batch_results, fn result ->
      changeset = changesets_by_index[result.__metadata__.bulk_create_index]

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
         resource
       ) do
    results =
      Enum.flat_map(batch, fn result ->
        changeset = changesets_by_index[result.__metadata__.bulk_create_index]

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

    changes
    |> run_bulk_after_changes(
      all_changes,
      results,
      changesets_by_index,
      changesets,
      resource,
      opts,
      ref
    )
    |> then(fn records ->
      select =
        if opts[:select] do
          List.wrap(opts[:select])
        else
          resource |> Ash.Resource.Info.public_attributes() |> Enum.map(& &1.name)
        end

      case Ash.load(
             records,
             select,
             reuse_values?: true,
             domain: domain,
             actor: opts[:actor],
             authorize?: opts[:authorize?],
             tracer: opts[:tracer]
           ) do
        {:ok, records} ->
          Ash.load(
            records,
            List.wrap(opts[:load]),
            domain: domain,
            reuse_values?: true,
            actor: opts[:actor],
            authorize?: opts[:authorize?],
            tracer: opts[:tracer]
          )
          |> case do
            {:ok, records} ->
              {:ok, Enum.reject(records, & &1.__metadata__[:private][:missing_from_data_layer])}

            {:error, error} ->
              {:error, error}
          end

        other ->
          other
      end
    end)
    |> case do
      {:ok, records} ->
        records

      {:error, error} ->
        store_error(ref, error, opts)
        []
    end
  end

  defp run_bulk_after_changes(
         changes,
         all_changes,
         results,
         changesets_by_index,
         changesets,
         resource,
         opts,
         ref
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
        function_exported?(module, :batch_change, 3) &&
          function_exported?(module, :after_batch, 3) &&
          module.batch_callbacks?(changesets, change_opts, context)

      _ ->
        false
    end)
    |> Enum.reduce(results, fn {%{change: {module, change_opts}}, index}, results ->
      records = results

      if changes[index] == :all do
        results =
          Enum.map(results, fn result ->
            {changesets_by_index[result.__metadata__.bulk_create_index], result}
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
              result.__metadata__.bulk_create_index in List.wrap(changes[index])

            _ ->
              false
          end)

        matches =
          Enum.map(matches, fn match ->
            {changesets_by_index[match.__metadata__.bulk_create_index], match}
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
          |> handle_after_batch_results(matches, ref, resource, opts)

        Enum.concat([after_batch_results, non_matches])
      end
    end)
  end

  defp handle_after_batch_results(:ok, matches, _, _resource, _), do: matches

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
      action: changeset.action,
      for: Ash.Resource.Info.notifiers(changeset.resource) ++ changeset.action.notifiers,
      data: result,
      changeset: changeset
    }
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

  defp run_action_changes(batch, all_changes, _action, actor, authorize?, tracer, tenant) do
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
                      Ash.Changeset.add_error(changeset, validation.message || error)
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
                (function_exported?(module, :batch_change, 3) &&
                   function_exported?(module, :after_batch, 3) &&
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
                  (function_exported?(module, :batch_change, 3) &&
                     function_exported?(module, :after_batch, 3) &&
                     module.batch_callbacks?(batch, change_opts, context))

              %{
                state
                | must_return_records?: must_return_records?,
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
    case change_opts do
      {:templated, change_opts} ->
        if function_exported?(module, :batch_change, 4) do
          module.batch_change(
            batch,
            change_opts,
            struct(struct(Ash.Resource.Change.Context, context), bulk?: true),
            actor
          )
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
end
