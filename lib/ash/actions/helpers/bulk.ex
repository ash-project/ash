# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Helpers.Bulk do
  @moduledoc false

  require Logger

  @typedoc """
  Tagged result tuple carrying a record/error with its associated changeset.

  Used throughout bulk operations to maintain changeset context for hook execution.
  """
  @type tagged_result ::
          {:ok, Ash.Resource.record(), Ash.Changeset.t()}
          | {:error, term(), Ash.Changeset.t()}

  @typedoc """
  Extended tagged result that includes hook completion markers.

  The `_hooks_done` variants indicate after_transaction hooks have already run,
  preventing double execution during result processing.
  """
  @type tagged_result_with_hooks ::
          tagged_result()
          | {:ok_hooks_done, Ash.Resource.record(), Ash.Changeset.t()}
          | {:error_hooks_done, term(), Ash.Changeset.t()}

  @doc """
  Conditionally rolls back transaction for bulk operations.

  Only rolls back if `opts[:transaction]` and `opts[:rollback_on_error?]` are set
  and we're inside a transaction. Returns the error unchanged for piping.
  """
  @spec maybe_rollback(error, Ash.Resource.t(), Keyword.t()) :: error when error: term()
  def maybe_rollback(error, resource, opts) do
    if opts[:transaction] && opts[:rollback_on_error?] do
      if Ash.DataLayer.in_transaction?(resource) do
        Ash.DataLayer.rollback(resource, error)
      end
    end

    error
  end

  @doc """
  Conditionally stops bulk operation on error.

  Throws if `stop_on_error?` is set and `return_stream?` is false.
  Otherwise returns the error unchanged for piping.
  """
  @spec maybe_stop_on_error(error, Keyword.t()) :: error | no_return() when error: term()
  def maybe_stop_on_error(error, opts) do
    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error)})
    end

    error
  end

  @doc """
  Looks up the changeset for a result record using ref metadata.

  First tries ref metadata (new), falls back to index->ref lookup (legacy).
  """
  def lookup_changeset(result, changesets_by_ref, changesets_by_index, opts) do
    index_key = opts[:index_key]
    ref_key = opts[:ref_key] || :bulk_action_ref

    result.__metadata__
    |> get_ref_from_metadata(ref_key, index_key, changesets_by_index)
    |> then(&changesets_by_ref[&1])
  end

  defp get_ref_from_metadata(metadata, ref_key, index_key, changesets_by_index) do
    with nil <- metadata[ref_key],
         index when not is_nil(index) <- metadata[index_key],
         ref when not is_nil(ref) <- changesets_by_index[index] do
      ref
    else
      ref when not is_nil(ref) -> ref
      _ -> nil
    end
  end

  @doc """
  Sets bulk operation metadata on a result record from a changeset.

  Uses pattern matching to extract index/ref from changeset context.
  """
  def put_metadata(result, %{context: %{bulk_create: %{index: index, ref: ref}}}) do
    result
    |> Ash.Resource.put_metadata(:bulk_create_index, index)
    |> maybe_put_ref_metadata(ref, :bulk_action_ref)
  end

  def put_metadata(result, %{context: %{bulk_destroy: %{index: index, ref: ref}}}) do
    result
    |> Ash.Resource.put_metadata(:bulk_destroy_index, index)
    |> maybe_put_ref_metadata(ref, :bulk_action_ref)
  end

  def put_metadata(result, %{context: %{bulk_update: %{index: index, ref: ref}}}) do
    result
    |> Ash.Resource.put_metadata(:bulk_update_index, index)
    |> maybe_put_ref_metadata(ref, :bulk_action_ref)
  end

  def put_metadata(result, _changeset), do: result

  @doc """
  Sets bulk operation metadata with explicit values (for batch operations).

  Used when index/ref are already extracted as separate variables.
  """
  def put_metadata(result, index, ref, index_key) when is_integer(index) do
    result
    |> Ash.Resource.put_metadata(index_key, index)
    |> maybe_put_ref_metadata(ref, :bulk_action_ref)
  end

  defp maybe_put_ref_metadata(result, ref, ref_key) do
    if Application.get_env(:ash, :test_bulk_index_only, false) do
      result
    else
      Ash.Resource.put_metadata(result, ref_key, ref)
    end
  end

  @doc """
  Validates multitenancy requirements for bulk operations.

  Checks if tenant is required based on resource configuration, action settings,
  and context overrides. Returns `:ok` or an error with `TenantRequired` exception.
  """
  @spec validate_multitenancy(Ash.Resource.t(), Ash.Resource.Actions.action(), keyword()) ::
          :ok | {:error, Exception.t()}
  def validate_multitenancy(resource, action, opts) do
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

  @doc """
  Marks that at least one record succeeded in a bulk operation.

  Only sets to true, never overwrites true with false.
  This ensures partial_success status when some batches succeed and others fail.
  """
  @spec set_success(reference(), atom()) :: :ok
  def set_success(ref, status) when status != :error do
    Process.put({:any_success?, ref}, true)
    :ok
  end

  def set_success(_ref, :error), do: :ok

  @doc """
  Stores notification in process dictionary for bulk operations.
  """
  @spec store_notification(reference(), list() | term() | nil, Keyword.t()) :: :ok
  def store_notification(_ref, empty, _opts) when empty in [[], nil], do: :ok

  def store_notification(ref, notification, opts) do
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

    :ok
  end

  @doc """
  Accumulates errors for bulk operation results.

  Returns `{error_count, errors}` tuple.
  """
  @spec errors(Ash.BulkResult.t(), term(), Keyword.t()) :: {non_neg_integer(), list()}
  def errors(result, invalid, opts) when is_list(invalid) do
    Enum.reduce(invalid, {result.error_count, result.errors}, fn invalid, {error_count, errors} ->
      errors(%{result | error_count: error_count, errors: errors}, invalid, opts)
    end)
  end

  def errors(result, nil, _opts) do
    {result.error_count + 1, []}
  end

  def errors(result, {:error, error}, opts) do
    if opts[:return_errors?] do
      {result.error_count + 1, [error | List.wrap(result.errors)]}
    else
      {result.error_count + 1, []}
    end
  end

  def errors(result, invalid, opts) do
    if Enumerable.impl_for(invalid) do
      invalid = Enum.to_list(invalid)
      errors(result, invalid, opts)
    else
      errors(result, {:error, invalid}, opts)
    end
  end

  @doc """
  Splits a list of changesets into valid and invalid ones.

  Returns a tuple where the first element contains valid changesets and the second
  contains error tuples for invalid changesets in the form `{:error, error, changeset}`.

  If `stop_on_error?` is true and `return_stream?` is false, throws on the first
  invalid changeset encountered.
  """
  @spec split_valid_invalid_changesets([Ash.Changeset.t()], Keyword.t()) ::
          {[Ash.Changeset.t()], [{:error, Ash.Error.t(), Ash.Changeset.t()}]}
  def split_valid_invalid_changesets(changesets, _opts) do
    changesets
    |> Enum.reduce({[], []}, fn
      %{valid?: false} = changeset, {batch_acc, results_acc} ->
        # in case of stop_on_error? we need to throw from here
        # and stop processing further changesets
        error = Ash.Error.to_error_class(changeset.errors, changeset: changeset)

        {batch_acc, [{:error, error, changeset} | results_acc]}

      changeset, {batch_acc, results_acc} ->
        {[changeset | batch_acc], results_acc}
    end)
    |> then(fn {batch, invalid_changeset_errors} ->
      {Enum.reverse(batch), Enum.reverse(invalid_changeset_errors)}
    end)
  end

  @doc """
  Checks if notifications need to be tracked for this bulk operation.
  """
  @spec need_notifications?(Keyword.t()) :: boolean()
  def need_notifications?(opts) do
    opts[:notify?] || opts[:return_notifications?]
  end

  @typedoc """
  Map correlating changeset references to their changesets.

  Built during result processing to enable matching loaded records back to
  their original changesets via the `bulk_changeset_id` metadata.
  """
  @type changeset_by_id :: %{reference() => Ash.Changeset.t()}

  @typedoc """
  Function that creates a notification from a changeset and loaded record.
  """
  @type notification_fn ::
          (Ash.Changeset.t(), Ash.Resource.record(), Keyword.t() ->
             Ash.Notifier.Notification.t())

  @doc """
  Stores notifications for loaded records by matching via changeset_id in metadata.

  Uses a unique reference (changeset_id) to correlate loaded records back to their
  original changesets, which works for all resources including those without primary keys.

  Buffers notifications in the process dictionary via `store_notification/3`.
  """
  @spec store_notifications_for_loaded_records(
          loaded_records :: [Ash.Resource.record()],
          changeset_by_id(),
          bulk_ref :: reference(),
          notification_fn(),
          opts :: Keyword.t()
        ) :: :ok
  def store_notifications_for_loaded_records(
        loaded_records,
        changeset_by_id,
        ref,
        notification_fn,
        opts
      ) do
    if need_notifications?(opts) do
      Enum.each(loaded_records, fn loaded ->
        changeset_id = loaded.__metadata__[:bulk_changeset_id]

        case Map.fetch(changeset_by_id, changeset_id) do
          {:ok, changeset} ->
            store_notification(ref, notification_fn.(changeset, loaded, opts), opts)

          :error ->
            Logger.warning("""
            Bulk operation: Could not find changeset for loaded record.
            bulk_changeset_id: #{inspect(changeset_id)}
            This may indicate a bug in bulk operation result processing.
            """)
        end
      end)
    end

    :ok
  end

  @doc """
  Removes internal bulk_changeset_id metadata from records before returning.

  The bulk_changeset_id is an internal implementation detail used for matching
  loaded records back to their changesets. It should not be exposed to users.
  """
  @spec clean_changeset_id_metadata(list()) :: list()
  def clean_changeset_id_metadata(records) when is_list(records) do
    Enum.map(records, &clean_changeset_id_metadata/1)
  end

  def clean_changeset_id_metadata(record) when is_struct(record) do
    case record.__metadata__[:bulk_changeset_id] do
      nil -> record
      _id -> Map.update!(record, :__metadata__, &Map.delete(&1, :bulk_changeset_id))
    end
  end

  def clean_changeset_id_metadata(other), do: other
end
