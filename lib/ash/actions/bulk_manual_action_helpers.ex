# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.BulkManualActionHelpers do
  @moduledoc """
    Helper functions used for handling manual actions when used in bulk operations.
  """

  @doc """
  Processes the results of a manual action's bulk function.

  Returns tagged tuples `{:ok, result, changeset}` or `{:error, error, changeset}`
  that can be processed by `process_results` to run after_transaction hooks.
  """
  # sobelow_skip ["DOS.BinToAtom"]
  def process_bulk_results(
        results,
        manual_action_module,
        bulk_action_type,
        store_notification,
        ref,
        opts,
        batch,
        changesets_by_ref,
        changesets_by_index
      ) do
    index_key = :"#{bulk_action_type}_index"

    case results do
      :ok ->
        if opts[:return_records?] do
          raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned :ok without a result when `return_records?` is true"
        else
          []
        end

      results when is_list(results) ->
        tagged_results =
          Enum.flat_map(results, fn
            :ok ->
              if opts[:return_records?] do
                raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned :ok without a result when `return_records?` is true"
              else
                []
              end

            # Already tagged with changeset (from process_non_bulk_result)
            {:ok, result, %Ash.Changeset{} = changeset} ->
              [{:ok, result, changeset}]

            {:ok, result} ->
              changeset =
                lookup_changeset(result, changesets_by_ref, changesets_by_index, index_key)

              [{:ok, result, changeset}]

            {:ok, result, %{notifications: notifications}} ->
              store_notification.(ref, notifications, opts)

              changeset =
                lookup_changeset(result, changesets_by_ref, changesets_by_index, index_key)

              [{:ok, result, changeset}]

            {:ok, result, notifications} when is_list(notifications) ->
              store_notification.(ref, notifications, opts)

              changeset =
                lookup_changeset(result, changesets_by_ref, changesets_by_index, index_key)

              [{:ok, result, changeset}]

            {:notifications, notifications} ->
              if opts[:return_records?] do
                raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned {:notifications, notifications} without a result when `return_records?` is true"
              else
                []
              end

              store_notification.(ref, notifications, opts)
              []

            # Already tagged with changeset (from process_non_bulk_result)
            {:error, error, %Ash.Changeset{} = changeset} ->
              [{:error, error, changeset}]

            {:error, error} ->
              # Associate error with all changesets in the batch (like the normal path)
              batch |> Enum.map(&{:error, error, &1})
          end)

        {:manual_tagged, tagged_results}

      {:error, error} ->
        # Associate error with all changesets in the batch
        {:manual_tagged, batch |> Enum.map(&{:error, error, &1})}

      {:notifications, notifications} ->
        store_notification.(ref, notifications, opts)
        []
    end
  end

  defp lookup_changeset(result, changesets_by_ref, changesets_by_index, index_key) do
    Ash.Actions.Helpers.lookup_changeset(
      result,
      changesets_by_ref,
      changesets_by_index,
      index_key: index_key,
      ref_key: :bulk_action_ref
    )
  end

  @doc """
  Used when a manual action does not export a bulk version of the action.

  Returns a tagged tuple `{:ok, result, changeset}` or `{:error, error, changeset}`
  that can be processed by `process_results` to run after_transaction hooks.
  """
  # sobelow_skip ["DOS.BinToAtom"]
  def process_non_bulk_result(
        result,
        changeset,
        bulk_action_type,
        store_notification \\ nil,
        ref \\ nil,
        opts \\ []
      ) do
    metadata_index_name = :"#{bulk_action_type}#{:_index}"

    case result do
      {:ok, record} ->
        record =
          record
          |> Ash.Resource.put_metadata(
            metadata_index_name,
            changeset.context[bulk_action_type].index
          )
          |> Ash.Resource.put_metadata(
            :bulk_action_ref,
            changeset.context[bulk_action_type].ref
          )

        {:ok, record, changeset}

      {:ok, record, notifications} ->
        if store_notification do
          store_notification.(ref, notifications, opts)
        end

        record =
          record
          |> Ash.Resource.put_metadata(
            metadata_index_name,
            changeset.context[bulk_action_type].index
          )
          |> Ash.Resource.put_metadata(
            :bulk_action_ref,
            changeset.context[bulk_action_type].ref
          )

        {:ok, record, changeset}

      {:error, error} ->
        {:error, error, changeset}
    end
  end

  @doc """
  Builds a BulkResult from processed results.

  Takes a list of records and `{:error, error}` tuples (output from `process_results`),
  along with success tracking and notifications, and builds the final BulkResult.
  """
  @spec build_bulk_result(
          processed_results :: [Ash.Resource.record() | {:error, term()}],
          any_success? :: boolean(),
          notifications :: [Ash.Notifier.Notification.t()],
          opts :: Keyword.t()
        ) :: Ash.BulkResult.t()
  def build_bulk_result(processed_results, any_success?, notifications, opts) do
    {records, error_tuples} =
      Enum.split_with(processed_results, fn
        {:error, _} -> false
        _ -> true
      end)

    errors =
      if opts[:return_errors?] do
        Enum.map(error_tuples, fn {:error, e} -> e end)
      else
        nil
      end

    has_records? = records != []
    has_errors? = error_tuples != []

    status =
      case {has_errors?, has_records? || any_success?} do
        {false, _} -> :success
        {true, false} -> :error
        {true, true} -> :partial_success
      end

    %Ash.BulkResult{
      status: status,
      records: if(opts[:return_records?], do: records, else: []),
      errors: errors,
      error_count: length(error_tuples),
      notifications: notifications
    }
  end
end
