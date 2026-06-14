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

  def run(domain, resource, action, inputs, opts) do
    action = get_action!(resource, action)
    pkey = Ash.Resource.Info.primary_key(resource)
    opts = Keyword.put(opts, :domain, domain)
    strategy = List.wrap(opts[:strategy] || [:atomic_batches])
    changeset_opts = changeset_opts(domain, opts)

    inputs
    |> batches(resource, action, strategy, opts)
    |> Enum.reduce(initial_accumulator(opts), fn batch, acc ->
      {records, errors} =
        run_batch(resource, action, pkey, strategy, changeset_opts, batch, opts)

      accumulate(acc, records, errors, opts)
    end)
    |> finalize(opts)
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
    # strategy. Everything it can't take (non-atomic or invalid changesets, or every change when the
    # data layer can't `update_many`) flows to `Ash.bulk_update`, which enforces the strategy itself.
    use_update_many? =
      Ash.DataLayer.data_layer_can?(resource, :update_many) and
        (:atomic in strategy or :atomic_batches in strategy)

    {atomic, fallback} =
      if use_update_many? do
        Enum.split_with(built, fn target ->
          match?(%Ash.Changeset{valid?: true}, target.changeset)
        end)
      else
        {[], built}
      end

    Ash.DataLayer.transaction(
      resource,
      fn ->
        atomic_updated = run_atomic(resource, action, atomic, opts)
        manual_results = run_manual(resource, action, strategy, fallback, opts)

        assemble_batch(resource, pkey, atomic, atomic_updated, manual_results)
      end,
      nil,
      %{type: :bulk_update, metadata: %{resource: resource, action: action.name}}
    )
    |> case do
      {:ok, {records, errors}} -> {records, errors}
      {:error, error} -> {[], [Ash.Error.to_ash_error(error)]}
    end
  end

  defp run_atomic(_resource, _action, [], _opts), do: []

  defp run_atomic(resource, action, atomic, opts) do
    atomic
    |> Enum.group_by(fn %{changeset: changeset} -> {changeset.atomics, changeset.filter} end)
    |> Enum.flat_map(fn {_group_key, targets} ->
      changesets = Enum.map(targets, & &1.changeset)

      # We always need the primary keys back (to diff updated rows against the inputs for
      # NotFound/Stale), but only load the full record set when the caller actually wants records.
      action_select =
        if opts[:return_records?] do
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
    end)
  end

  defp run_manual(_resource, _action, _strategy, [], _opts), do: []

  defp run_manual(resource, action, strategy, manual, opts) do
    # `Ash.bulk_update`'s own strategy handling isn't uniform (it will stream an identifier query
    # even when `:stream` wasn't requested, and force-streams on data layers without atomic update
    # support), so enforce the strategy here instead of relying on it. A change reaches this path
    # only because the single-statement `update_many` couldn't take it; we can still honor an
    # atomic-only strategy when the data layer can update atomically on its own.
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

    invalid_results =
      Enum.map(invalid, fn target ->
        {:error, target, Ash.Error.to_ash_error(target.changeset.errors)}
      end)

    runnable_results =
      Enum.map(runnable, &run_via_bulk_update(resource, action, strategy, &1, opts))

    # Being unable to satisfy an atomic-only strategy is a single property of the request, not a
    # per-row data problem, so the whole blocked set collapses to one error.
    blocked_result =
      case blocked do
        [] -> []
        [target | _] -> [{:error, target, no_matching_strategy_error(resource, action, strategy)}]
      end

    invalid_results ++ runnable_results ++ blocked_result
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

    query_or_records
    |> Ash.bulk_update(action.name, target.input, manual_bulk_opts(strategy, opts))
    |> case do
      %Ash.BulkResult{status: :success, records: [record | _]} ->
        {:ok, record}

      %Ash.BulkResult{status: :success} ->
        # Nothing matched: the record is gone, or the identifier names no row.
        {:error, target, missing_error(resource, target)}

      %Ash.BulkResult{errors: errors} ->
        {:error, target, Ash.Error.to_ash_error(errors)}
    end
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

  defp assemble_batch(resource, pkey, atomic_targets, atomic_records, manual_results) do
    returned_pkeys = MapSet.new(atomic_records, &Map.take(&1, pkey))

    atomic_errors =
      atomic_targets
      |> Enum.reject(&MapSet.member?(returned_pkeys, &1.pkey))
      |> Enum.map(&missing_error(resource, &1))

    {manual_records, manual_errors} =
      Enum.reduce(manual_results, {[], []}, fn
        {:ok, record}, {records, errors} ->
          {[record | records], errors}

        {:error, _target, error}, {records, errors} ->
          {records, [Ash.Error.to_ash_error(error) | errors]}
      end)

    {atomic_records ++ Enum.reverse(manual_records), atomic_errors ++ Enum.reverse(manual_errors)}
  end

  defp initial_accumulator(opts) do
    %{
      success_count: 0,
      error_count: 0,
      records: if(opts[:return_records?], do: [], else: nil),
      errors: if(opts[:return_errors?], do: [], else: nil)
    }
  end

  # Records/errors are accumulated as a reversed list-of-batches (then flattened in `finalize/2`)
  # so appending stays O(1) per batch rather than O(n) — important when there are many batches.
  defp accumulate(acc, records, errors, opts) do
    %{
      success_count: acc.success_count + length(records),
      error_count: acc.error_count + length(errors),
      records: if(opts[:return_records?], do: [records | acc.records], else: nil),
      errors: if(opts[:return_errors?], do: [errors | acc.errors], else: nil)
    }
  end

  defp finalize(acc, _opts) do
    %Ash.BulkResult{
      status: status(acc.success_count, acc.error_count),
      error_count: acc.error_count,
      records: acc.records && acc.records |> Enum.reverse() |> Enum.concat(),
      errors: acc.errors && acc.errors |> Enum.reverse() |> Enum.concat()
    }
  end

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

  defp normalize_target!(resource, target, pkey) do
    cond do
      is_struct(target) and target.__struct__ == resource ->
        {:record, target}

      is_map(target) and not is_struct(target) ->
        {:identifier, struct(resource, Map.take(target, pkey))}

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
    [
      domain: opts[:domain],
      return_records?: true,
      return_errors?: true,
      stop_on_error?: false,
      strategy: strategy
    ]
    |> maybe_put(:actor, opts[:actor])
    |> maybe_put(:tenant, opts[:tenant])
    |> maybe_put(:authorize?, opts[:authorize?])
    |> maybe_put(:context, opts[:context])
  end

  defp maybe_put(opts, _key, nil), do: opts
  defp maybe_put(opts, key, value), do: Keyword.put(opts, key, value)
end
