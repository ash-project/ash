# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Update.UpdateMany do
  @moduledoc false
  # Updates many records, each with its own input.
  #
  # The `:strategy` option controls how the work is executed (default `[:atomic_batches]`):
  #
  #   * `:atomic` — apply the whole input in a single pass, one statement per shared atomics/filter
  #     group (e.g. one SQL `MERGE`). No batching.
  #   * `:atomic_batches` — same, but chunk the input into `:batch_size` batches first, so large
  #     inputs stay within the data layer's bind-parameter limits. This is the default.
  #   * `:stream` — update records one at a time via `Ash.bulk_update`, for changes that can't be
  #     made atomic (or data layers without `update_many`).
  #
  # Strategies are tried in the order `:atomic`, `:atomic_batches`, `:stream`; only those present are
  # allowed. Without `:stream`, a change that can't be applied atomically fails the operation with a
  # single error rather than silently falling back to streaming (this is a property of the request,
  # not of any one row — unlike the per-row stale/not-found errors below).
  #
  # Inputs are `{record_or_identifier, input}` tuples. A record that no longer exists (or whose
  # input was a bare identifier with no matching row) becomes a per-row error in the result:
  # `Ash.Error.Changes.StaleRecord` for a record, `Ash.Error.Query.NotFound` for an identifier.
  #
  # Notifications (`:notify?` / `:return_notifications?`) work like other bulk actions: built per
  # updated record (the atomic path emits resource-level notifier notifications), accumulated across
  # batches, and only sent once every batch has committed — or handed to the enclosing Ash
  # transaction's owner when nested inside one.

  def run(domain, resource, action, inputs, opts) do
    action = get_action!(resource, action)
    pkey = Ash.Resource.Info.primary_key(resource)
    opts = Ash.Actions.Helpers.apply_scope_to_opts(opts) |> Keyword.put(:domain, domain)
    strategy = List.wrap(opts[:strategy] || [:atomic_batches])
    changeset_opts = changeset_opts(domain, opts)

    inputs
    |> batches(resource, action, strategy, opts)
    |> Enum.reduce(initial_accumulator(opts), fn batch, acc ->
      {records, errors, notifications} =
        run_batch(resource, action, pkey, strategy, changeset_opts, batch, opts)

      accumulate(acc, records, errors, notifications, opts)
    end)
    |> finalize(opts)
    |> process_notifications(resource, action, opts)
  end

  # Only `:atomic_batches` chunks the input. Otherwise the whole input is handled in a single pass
  # (one statement per atomics/filter group), which is the default.
  defp batches(inputs, resource, action, strategy, opts) do
    if :atomic_batches in strategy do
      Stream.chunk_every(inputs, batch_size(resource, action, opts))
    else
      [Enum.to_list(inputs)]
    end
  end

  defp run_batch(resource, action, pkey, strategy, changeset_opts, batch, opts) do
    targets =
      Enum.map(batch, fn {target, input} ->
        {kind, record} = normalize_target!(resource, target, pkey)
        %{kind: kind, record: record, input: input || %{}, pkey: Map.take(record, pkey)}
      end)

    built =
      Enum.map(targets, fn target ->
        changeset =
          Ash.Changeset.fully_atomic_changeset(
            resource,
            action,
            target.input,
            Keyword.put(changeset_opts, :data, target.record)
          )

        Map.put(target, :changeset, changeset)
      end)

    # The single-statement path needs both an `update_many`-capable data layer and an atomic-ish
    # strategy. Everything it can't take (non-atomic or invalid changesets, changesets whose hooks
    # the MERGE path can't run, or every change when the data layer can't `update_many`) flows to
    # `Ash.bulk_update`, which enforces the strategy and runs those hooks the same atomic way.
    use_update_many? =
      Ash.DataLayer.data_layer_can?(resource, :update_many) and
        (:atomic in strategy or :atomic_batches in strategy)

    {atomic, fallback} =
      if use_update_many? do
        Enum.split_with(built, fn target ->
          match?(%Ash.Changeset{valid?: true}, target.changeset) and
            merge_runnable?(target.changeset)
        end)
      else
        {[], built}
      end

    Ash.DataLayer.transaction(
      resource,
      fn ->
        {atomic_records, atomic_errors, atomic_notifications} =
          run_atomic(resource, action, pkey, atomic, opts)

        {manual_records, manual_errors, manual_notifications} =
          run_manual(resource, action, strategy, fallback, opts)

        {atomic_records ++ manual_records, atomic_errors ++ manual_errors,
         atomic_notifications ++ manual_notifications}
      end,
      nil,
      %{type: :bulk_update, metadata: %{resource: resource, action: action.name}}
    )
    |> case do
      {:ok, {records, errors, notifications}} -> {records, errors, notifications}
      {:error, error} -> {[], [Ash.Error.to_ash_error(error)], []}
    end
  end

  defp run_atomic(_resource, _action, _pkey, [], _opts), do: {[], [], []}

  defp run_atomic(resource, action, pkey, atomic, opts) do
    notify? = notify?(opts)

    # after_action hooks (and notification subscribers) need the full updated record, not just pkeys.
    has_after_action? = Enum.any?(atomic, &(&1.changeset.after_action != []))

    {records, errors, notifications, returned_pkeys} =
      atomic
      |> Enum.group_by(fn %{changeset: changeset} -> {changeset.atomics, changeset.filter} end)
      |> Enum.reduce({[], [], [], MapSet.new()}, fn {_group_key, targets}, acc ->
        {records_acc, errors_acc, notifications_acc, seen} = acc
        changesets = Enum.map(targets, & &1.changeset)

        # Changesets in a group share atomics/filter; use the first as the batch's representative for
        # the after_batch change callbacks, exactly as an atomic bulk update uses one changeset.
        representative = hd(changesets)

        context =
          struct(Ash.Resource.Change.Context, %{
            bulk?: true,
            source_context: representative.context,
            actor: opts[:actor],
            tenant: opts[:tenant],
            tracer: opts[:tracer],
            authorize?: opts[:authorize?]
          })

        # Only *unconditional* after_batch hooks reach the merge path; conditional ones (whose `where`
        # is a calculation needing both the old and new row values together) are routed to the
        # `bulk_update` fallback. So `conditional_after_batch_hooks` here only ever has the `true`
        # condition, which `run_atomic_after_batch_hooks` applies to every row without a calculation.
        {all_changes, conditional_after_batch_hooks, _calculations} =
          Ash.Actions.Update.Bulk.hooks_and_calcs_for_update_query(
            representative,
            context,
            Ash.Query.new(resource),
            opts
          )

        action_select =
          if opts[:return_records?] or notify? or has_after_action? or
               not Enum.empty?(conditional_after_batch_hooks) do
            action_select(resource, action, opts)
          else
            Ash.Resource.Info.primary_key(resource)
          end

        data_layer_opts = %{
          return_records?: true,
          tenant: opts[:tenant],
          action_select: action_select,
          calculations: []
        }

        written =
          case Ash.DataLayer.update_many(resource, changesets, data_layer_opts) do
            :ok ->
              []

            {:ok, records} ->
              records

            {:partial_success, _failed, records} ->
              records

            {:error, error} ->
              Ash.DataLayer.rollback(resource, Ash.Error.to_ash_error(error))

            {:error, :no_rollback, error} ->
              Ash.DataLayer.rollback(resource, Ash.Error.to_ash_error(error))
          end

        # after_batch change callbacks, run the same way an atomic bulk update runs them.
        {written, after_batch_notifications} =
          if Enum.empty?(conditional_after_batch_hooks) do
            {written, []}
          else
            Ash.Actions.Update.Bulk.run_atomic_after_batch_hooks(
              written,
              representative,
              all_changes,
              conditional_after_batch_hooks,
              context
            )
          end

        changeset_by_pkey = Map.new(targets, &{&1.pkey, &1.changeset})

        # Run after_action hooks per row with that row's own changeset — the same hooks atomic bulk
        # updates run after their write. The MERGE already happened, so a hook failure is a per-row
        # error (the row stays updated) rather than a rollback, matching bulk update behavior.
        {group_records, group_errors, group_notifications, seen} =
          Enum.reduce(written, {[], [], [], seen}, fn record, {recs, errs, notifs, seen} ->
            row_pkey = Map.take(record, pkey)
            seen = MapSet.put(seen, row_pkey)
            changeset = Map.get(changeset_by_pkey, row_pkey) || representative

            case Ash.Changeset.run_after_actions(record, changeset, []) do
              {:ok, new_record, _changeset, %{notifications: hook_notifications}} ->
                row_notifications =
                  if notify? do
                    [
                      Ash.Actions.Helpers.resource_notification(changeset, new_record, opts)
                      | hook_notifications
                    ]
                  else
                    []
                  end

                {[new_record | recs], errs, row_notifications ++ notifs, seen}

              {:error, error} ->
                {recs, [Ash.Error.to_ash_error(error) | errs], notifs, seen}
            end
          end)

        group_notifications =
          if notify?,
            do: after_batch_notifications ++ group_notifications,
            else: group_notifications

        {records_acc ++ Enum.reverse(group_records), errors_acc ++ Enum.reverse(group_errors),
         notifications_acc ++ group_notifications, seen}
      end)

    # Targets whose row never came back from the MERGE are genuinely missing.
    missing_errors =
      atomic
      |> Enum.reject(&MapSet.member?(returned_pkeys, &1.pkey))
      |> Enum.map(&missing_error(resource, &1))

    {records, errors ++ missing_errors, notifications}
  end

  defp run_manual(_resource, _action, _strategy, [], _opts), do: {[], [], []}

  defp run_manual(resource, action, strategy, manual, opts) do
    # `Ash.bulk_update`'s own strategy handling isn't uniform (it will stream an identifier query
    # even when `:stream` wasn't requested, and force-streams on data layers without atomic update
    # support), so enforce the strategy here instead of relying on it. A change reaches this path
    # because the single-statement `update_many` couldn't take it; `bulk_update` then runs the full
    # pipeline (before/after batch, after_action, after_transaction) the same atomic way.
    stream? = :stream in strategy
    atomic_capable? = Ash.DataLayer.data_layer_can?(resource, :update_query)

    # An invalid changeset is a genuine per-row validation failure. Everything else either runs (the
    # data layer can apply it, or `:stream` permits per-record updates) or is blocked by the strategy.
    {invalid, runnable_or_blocked} =
      Enum.split_with(manual, &match?(%Ash.Changeset{valid?: false}, &1.changeset))

    {runnable, blocked} =
      Enum.split_with(runnable_or_blocked, fn target ->
        stream? or (atomic_capable? and match?(%Ash.Changeset{valid?: true}, target.changeset))
      end)

    invalid_errors =
      Enum.map(invalid, &Ash.Error.to_ash_error(&1.changeset.errors))

    {runnable_records, runnable_errors, runnable_notifications} =
      Enum.reduce(runnable, {[], [], []}, fn target, {records, errors, notifications} ->
        {result, notifs} = run_via_bulk_update(resource, action, strategy, target, opts)

        case result do
          {:ok, record} -> {[record | records], errors, notifs ++ notifications}
          {:error, _target, error} -> {records, [error | errors], notifs ++ notifications}
        end
      end)

    # Being unable to satisfy an atomic-only strategy is a single property of the request, not a
    # per-row data problem, so the whole blocked set collapses to one error.
    blocked_errors =
      case blocked do
        [] -> []
        _ -> [no_matching_strategy_error(resource, action, strategy)]
      end

    {Enum.reverse(runnable_records),
     invalid_errors ++ Enum.reverse(runnable_errors) ++ blocked_errors, runnable_notifications}
  end

  defp run_via_bulk_update(resource, action, strategy, target, opts) do
    # A record is updated in place; a bare identifier is fetched first. `Ash.bulk_update` only reads
    # when handed a query, so passing the record itself avoids a needless fetch, while the identifier
    # becomes a primary-key query that it reads for us.
    query_or_records =
      case target.kind do
        :record -> [target.record]
        :identifier -> Ash.Query.do_filter(resource, Map.to_list(target.pkey))
      end

    bulk_result =
      Ash.bulk_update(
        query_or_records,
        action.name,
        target.input,
        manual_bulk_opts(strategy, opts)
      )

    result =
      case bulk_result do
        %Ash.BulkResult{status: :success, records: [record | _]} ->
          {:ok, record}

        %Ash.BulkResult{status: :success} ->
          # Nothing matched: the record is gone, or the identifier names no row.
          {:error, target, missing_error(resource, target)}

        %Ash.BulkResult{errors: errors} ->
          {:error, target, Ash.Error.to_ash_error(errors)}
      end

    # `manual_bulk_opts` forces `return_notifications?: true`, so bulk_update returns rather than
    # sends; we fold them into the operation-wide set and emit/return them once at the end.
    {result, List.wrap(bulk_result.notifications)}
  end

  defp no_matching_strategy_error(resource, action, strategy) do
    Ash.Error.to_ash_error(
      Ash.Error.Invalid.NoMatchingBulkStrategy.exception(
        resource: resource,
        action: action.name,
        requested_strategies: strategy,
        not_atomic_reason:
          "the change cannot be applied atomically, and the data layer cannot update these records atomically",
        not_stream_reason: "`:stream` is not one of the requested strategies"
      )
    )
  end

  # The merge path runs the hooks an atomic bulk update runs after its write that need nothing the
  # MERGE can't provide: after_action and *unconditional* after_batch (applies to every row).
  #
  # Everything else goes to the `bulk_update` fallback, whose update is a join-capable `SELECT`:
  #   * before_batch — not part of an atomic write.
  #   * conditional hooks — a hook gated by a `where` needs that condition evaluated with both the
  #     old (`ref`) and new (`atomic_ref`) row values together, which only exist during the update
  #     statement; a MERGE can't surface that (its RETURNING can't join, and is post-update only).
  #   * after_transaction — runs post-commit.
  # (before_action already forces the changeset non-atomic, so it never reaches here.)
  defp merge_runnable?(%Ash.Changeset{} = changeset) do
    Enum.empty?(changeset.after_transaction) and
      not action_needs_fallback_for_hooks?(changeset.resource, changeset.action)
  end

  defp action_needs_fallback_for_hooks?(resource, action) do
    (action.changes ++ Ash.Resource.Info.changes(resource, action.type))
    |> Enum.any?(fn
      %{change: {module, _opts}} = change ->
        module.has_before_batch?() or
          (conditional_change?(change) and (module.has_after_batch?() or module.has_change?()))

      _ ->
        false
    end)
  end

  defp conditional_change?(%{where: where}), do: where not in [[], nil]
  defp conditional_change?(_), do: false

  defp initial_accumulator(opts) do
    %{
      success_count: 0,
      error_count: 0,
      records: if(opts[:return_records?], do: [], else: nil),
      errors: if(opts[:return_errors?], do: [], else: nil),
      notifications: []
    }
  end

  # Records/errors/notifications are accumulated as a reversed list-of-batches (then flattened in
  # `finalize/2`) so appending stays O(1) per batch rather than O(n) — important with many batches.
  defp accumulate(acc, records, errors, notifications, opts) do
    %{
      success_count: acc.success_count + length(records),
      error_count: acc.error_count + length(errors),
      records: if(opts[:return_records?], do: [records | acc.records], else: nil),
      errors: if(opts[:return_errors?], do: [errors | acc.errors], else: nil),
      notifications: [notifications | acc.notifications]
    }
  end

  defp finalize(acc, _opts) do
    %Ash.BulkResult{
      status: status(acc.success_count, acc.error_count),
      error_count: acc.error_count,
      records: acc.records && acc.records |> Enum.reverse() |> Enum.concat(),
      errors: acc.errors && acc.errors |> Enum.reverse() |> Enum.concat(),
      notifications: acc.notifications |> Enum.reverse() |> Enum.concat()
    }
  end

  # Notifications are built inside each batch's transaction but only emitted here, once everything
  # has committed. If we're nested inside another Ash transaction, hand them to that owner instead.
  defp process_notifications(result, resource, action, opts) do
    cond do
      opts[:return_notifications?] ->
        result

      Enum.empty?(result.notifications) ->
        %{result | notifications: nil}

      Process.get(:ash_started_transaction?, false) ->
        Process.put(
          :ash_notifications,
          List.wrap(Process.get(:ash_notifications)) ++ result.notifications
        )

        %{result | notifications: []}

      true ->
        remaining = Ash.Notifier.notify(result.notifications)

        Ash.Actions.Helpers.warn_missed!(resource, action, %{resource_notifications: remaining})

        %{result | notifications: []}
    end
  end

  defp notify?(opts), do: opts[:notify?] || opts[:return_notifications?] || false

  defp status(_success_count, 0), do: :success
  defp status(0, _error_count), do: :error
  defp status(_success_count, _error_count), do: :partial_success

  # Used under `:atomic_batches`. Each batch is a single MERGE, kept well under PostgreSQL's 65535
  # bind-param limit for typical updates; lower `:batch_size` for very wide updates.
  defp batch_size(_resource, _action, opts), do: opts[:batch_size] || 1000

  defp missing_error(resource, %{kind: :identifier, pkey: pkey}) do
    Ash.Error.to_ash_error(
      Ash.Error.Query.NotFound.exception(primary_key: pkey, resource: resource)
    )
  end

  defp missing_error(_resource, %{kind: :record}) do
    Ash.Error.to_ash_error(Ash.Error.Changes.StaleRecord.exception([]))
  end

  # Targets are matched by primary key (that's how the rows are grouped and how the data layer
  # matches them), so an identifier must be a record or the full primary key — not some other
  # identity. A partial/other map is rejected rather than silently matching nothing.
  defp normalize_target!(resource, target, pkey) do
    cond do
      is_struct(target) and target.__struct__ == resource ->
        {:record, target}

      is_map(target) and not is_struct(target) ->
        provided = Map.take(target, pkey)

        if map_size(provided) == length(pkey) do
          {:identifier, struct(resource, provided)}
        else
          raise ArgumentError,
                "Cannot use #{inspect(target)} as an identifier for #{inspect(resource)}: " <>
                  "`Ash.update_many` identifies records by their primary key #{inspect(pkey)}. " <>
                  "Pass a record or a map containing the full primary key."
        end

      match?([_], pkey) ->
        {:identifier, struct(resource, %{hd(pkey) => target})}

      true ->
        raise ArgumentError,
              "Cannot use #{inspect(target)} as an identifier for #{inspect(resource)}: provide a record or a map of the primary key #{inspect(pkey)}"
    end
  end

  defp get_action!(resource, action) do
    case action do
      %{} = action ->
        action

      name ->
        Ash.Resource.Info.action(resource, name) ||
          raise(
            Ash.Error.Invalid.NoSuchAction.exception(
              resource: resource,
              action: name,
              type: :update
            )
          )
    end
  end

  defp action_select(resource, _action, opts) do
    opts[:select] || Enum.map(Ash.Resource.Info.attributes(resource), & &1.name)
  end

  defp changeset_opts(domain, opts) do
    # Atomic update-default conditions ("only bump updated_at if a value changed") must reference
    # each row's incoming value via `upsert_conflict/1` rather than a baked-in literal, so the
    # resulting atomics are identical across the batch and can be applied in one statement.
    [domain: domain, reference_changes_via_conflict?: true]
    |> maybe_put(:actor, opts[:actor])
    |> maybe_put(:tenant, opts[:tenant])
    |> maybe_put(:authorize?, opts[:authorize?])
    |> maybe_put(:context, opts[:context])
  end

  defp manual_bulk_opts(strategy, opts) do
    # Pass the caller's strategy straight through: `Ash.bulk_update` decides whether each change can
    # be satisfied (e.g. errors under `[:atomic]` if a change isn't atomic, streams under `[:stream]`).
    # `return_notifications?: true` makes it hand notifications back rather than sending them, so we
    # can emit them once for the whole operation after every batch has committed.
    [
      domain: opts[:domain],
      return_records?: true,
      return_errors?: true,
      return_notifications?: notify?(opts),
      stop_on_error?: false,
      strategy: strategy
    ]
    |> maybe_put(:actor, opts[:actor])
    |> maybe_put(:tenant, opts[:tenant])
    |> maybe_put(:authorize?, opts[:authorize?])
    |> maybe_put(:context, opts[:context])
    |> maybe_put(:notification_metadata, opts[:notification_metadata])
  end

  defp maybe_put(opts, _key, nil), do: opts
  defp maybe_put(opts, key, value), do: Keyword.put(opts, key, value)
end
