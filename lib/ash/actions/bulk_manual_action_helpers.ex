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
    {index_value, metadata_key} = extract_bulk_metadata(changeset, bulk_action_type)

    case result do
      {:ok, record} ->
        record = Ash.Resource.put_metadata(record, metadata_key, index_value)
        {:ok, record}

      {:ok, record, notifications} ->
        record = Ash.Resource.put_metadata(record, metadata_key, index_value)
        {:ok, record, notifications}

      {:error, error} ->
        {:error, error}
    end
  end

  @doc """
  Extracts bulk operation index and metadata key from a changeset context.
  Used by manual actions to maintain proper ordering in bulk operations.

  Returns a tuple of `{index, metadata_key}` for the bulk operation.
  """
  def extract_bulk_metadata(changeset, bulk_action_type \\ :bulk_update) do
    changeset.context
    |> Enum.find_value(fn
      {{^bulk_action_type, ref}, value} -> {value.index, {:"#{bulk_action_type}_index", ref}}
      _ -> nil
    end)
  end
end
