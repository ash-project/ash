# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.BulkManualActionHelpers do
  @moduledoc """
  Helper functions used for handling manual actions when used in bulk operations.

  This module provides utilities for managing bulk operation context and metadata,
  with special emphasis on preventing context collisions in nested bulk operations.

  ## Context Collision Prevention

  Bulk operations use namespaced context keys to prevent collisions when operations
  are nested. Instead of simple atoms like `:bulk_create`, the system uses tuples
  like `{:bulk_create, ref}` where `ref` is a unique reference.

  ## Examples

  ### Basic Usage in Manual Actions

      def bulk_create(changesets, _opts, _context) do
        Enum.map(changesets, fn changeset ->
          # Extract the index for ordering
          index = BulkManualActionHelpers.get_changeset_index(changeset, :bulk_create)
          
          # Extract both index and metadata key for record tagging
          {index, metadata_key} = BulkManualActionHelpers.extract_bulk_metadata(changeset, :bulk_create)
          
          case create_record(changeset) do
            {:ok, record} ->
              # Tag the record with its bulk operation metadata
              {:ok, Ash.Resource.put_metadata(record, metadata_key, index)}
            error ->
              error
          end
        end)
      end

  ### Nested Bulk Operations

  The namespaced keys prevent context collisions in scenarios like:

      def bulk_create(changesets, _opts, _context) do
        Enum.map(changesets, fn changeset ->
          # This nested bulk_create won't interfere with the parent operation
          Ash.bulk_create!([%{related_field: "value"}], RelatedResource, :create)
          
          # Parent operation context remains intact
          {index, metadata_key} = BulkManualActionHelpers.extract_bulk_metadata(changeset, :bulk_create)
          create_and_tag_record(changeset, index, metadata_key)
        end)
      end

  ## Key Functions

  - `extract_bulk_metadata/2` - Gets both index and metadata key
  - `get_changeset_index/2` - Gets only the index (convenience function)
  - `get_changeset_metadata_key/2` - Gets only the metadata key (convenience function)
  - `create_bulk_operation_keys/1` - Creates namespaced context and metadata keys
  - `pair_changesets_with_results/3` - Matches results back to changesets
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
  # Handles context_key tuples from bulk operations
  def extract_bulk_metadata(changeset, {bulk_action_type, _ref}) do
    extract_bulk_metadata(changeset, bulk_action_type)
  end

  # Handles atoms from simple operations
  def extract_bulk_metadata(changeset, bulk_action_type) when is_atom(bulk_action_type) do
    changeset.context
    |> Enum.find_value(fn
      {{^bulk_action_type, ref}, value} ->
        metadata_atom =
          case bulk_action_type do
            :bulk_create -> :bulk_create_index
            :bulk_update -> :bulk_update_index
            :bulk_destroy -> :bulk_destroy_index
          end

        {value.index, {metadata_atom, ref}}

      _ ->
        nil
    end)
  end

  @doc """
  Creates unique namespaced keys for bulk operations to prevent collisions between
  nested operations. The context key stores operation data during processing,
  while the metadata key is attached to records for later index-based lookup.

  Returns `{context_key, metadata_key}` tuple.
  """
  def create_bulk_operation_keys(action_type) do
    ref = make_ref()

    case action_type do
      :create ->
        {{:bulk_create, ref}, {:bulk_create_index, ref}}

      :update ->
        {{:bulk_update, ref}, {:bulk_update_index, ref}}

      :destroy ->
        {{:bulk_destroy, ref}, {:bulk_destroy_index, ref}}
    end
  end

  @doc """
  Finds the bulk operation index value from a record's metadata, searching through
  all possible namespaced metadata keys for the given bulk action type.
  """
  def get_bulk_index(record, bulk_action_type \\ :bulk_update) do
    metadata_atom =
      case bulk_action_type do
        :bulk_create -> :bulk_create_index
        :bulk_update -> :bulk_update_index
        :bulk_destroy -> :bulk_destroy_index
      end

    record.__metadata__
    |> Enum.find_value(fn
      {{^metadata_atom, _ref}, index} -> index
      _ -> nil
    end)
  end

  @doc """
  Creates changeset-record pairs from results by looking up changesets using bulk indexes.
  Used by after-batch processing that needs to match records back to their original changesets.
  """
  def pair_changesets_with_results(results, changesets_by_index, metadata_key) do
    bulk_action_type =
      case metadata_key do
        {:bulk_create_index, _} -> :bulk_create
        {:bulk_update_index, _} -> :bulk_update
        {:bulk_destroy_index, _} -> :bulk_destroy
      end

    Enum.map(results, fn result ->
      index = get_bulk_index(result, bulk_action_type)
      {changesets_by_index[index], result}
    end)
  end

  @doc """
  Extracts only the bulk operation index from a changeset context.
  Convenience function for when only the index is needed.
  """
  def get_changeset_index(changeset, bulk_action_type) do
    {index, _} = extract_bulk_metadata(changeset, bulk_action_type)
    index
  end

  @doc """
  Extracts only the metadata key from a changeset context.
  Convenience function for when only the metadata key is needed.
  """
  def get_changeset_metadata_key(changeset, bulk_action_type) do
    {_, metadata_key} = extract_bulk_metadata(changeset, bulk_action_type)
    metadata_key
  end
end
