# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.BulkManualActionHelpers do
  @moduledoc """
    Helper functions used for handling manual actions when used in bulk operations.
  """

  @doc """
  Processes the results of a manual action's bulk function.
  """
  def process_bulk_results(
        results,
        manual_action_module,
        bulk_action_type,
        store_notification,
        store_error,
        ref,
        opts
      ) do
    case results do
      :ok ->
        if opts[:return_records?] do
          raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned :ok without a result when `return_records?` is true"
        else
          []
        end

      results when is_list(results) ->
        {:ok,
         Enum.reduce(results, [], fn
           :ok, results ->
             if opts[:return_records?] do
               raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned :ok without a result when `return_records?` is true"
             else
               results
             end

           {:ok, result}, results ->
             [result | results]

           {:ok, result, %{notifications: notifications}}, results ->
             store_notification.(ref, notifications, opts)
             [result | results]

           {:ok, result, notifications}, results ->
             store_notification.(ref, notifications, opts)
             [result | results]

           {:notifications, notifications}, results ->
             if opts[:return_records?] do
               raise "`#{inspect(manual_action_module)}.#{bulk_action_type}/3` returned {:notifications, notifications} without a result when `return_records?` is true"
             else
               results
             end

             store_notification.(ref, notifications, opts)
             results

           {:error, error}, results ->
             store_error.(ref, error, opts)
             results
         end)}

      {:error, error} ->
        store_error.(ref, error, opts)
        []

      {:notifications, notifications} ->
        store_notification.(ref, notifications, opts)
        []
    end
  end

  @doc """
  Used when a manual action does not export a bulk version of the action.
  """
  # sobelow_skip ["DOS.BinToAtom"]
  def process_non_bulk_result(
        result,
        changeset,
        bulk_action_type
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

        {:ok, record}

      {:ok, record, notifications} ->
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

        {:ok, record, notifications}

      {:error, error} ->
        {:error, error}
    end
  end
end
