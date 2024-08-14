defmodule Ash.BulkResult.BatchStatus do
  @moduledoc """
  The element value for bulk actions with streaming.
  """

  @type t :: %__MODULE__{
          inputs: pos_integer(),
          affected: non_neg_integer(),
          total_inputs: pos_integer(),
          total_affected: non_neg_integer()
        }

  @typedoc false
  @type unaccumulated() :: %__MODULE__{
          inputs: pos_integer(),
          affected: non_neg_integer(),
          total_inputs: nil,
          total_affected: nil
        }

  defstruct [
    :inputs,
    :affected,
    :total_inputs,
    :total_affected
  ]

  @doc false
  @spec accumulate(enumerable :: Enumerable.t(unaccumulated() | other)) ::
          Enumerable.t(t() | other)
        when other: term()
  def accumulate(enumerable) do
    Stream.transform(enumerable, {0, 0}, fn
      %__MODULE__{inputs: inputs, affected: affected} = status, {total_inputs, total_affected} ->
        total_inputs = total_inputs + inputs
        total_affected = total_affected + affected

        {[%__MODULE__{status | total_inputs: total_inputs, total_affected: total_affected}],
         {total_inputs, total_affected}}

      element, acc ->
        {[element], acc}
    end)
  end

  @doc false
  # When return_records? is false, a batch status is automatically appended to the batch stream
  # However when return_records? is true, we don't know the number of affected records until
  # the stream is executed.
  @spec tally_records(
          enumerable :: Enumerable.t(unaccumulated() | {:ok, record} | other),
          inputs :: non_neg_integer()
        ) :: Enumerable.t(unaccumulated() | {:ok, record} | other)
        when record: Ash.Resource.record(), other: term()
  def tally_records(enumerable, inputs) do
    Stream.transform(
      enumerable,
      fn -> %__MODULE__{inputs: inputs, affected: 0} end,
      fn
        {:ok, record}, acc ->
          acc =
            if Ash.Resource.get_metadata(record, :upsert_skipped),
              do: acc,
              else: %__MODULE__{acc | affected: acc.affected + 1}

          {[{:ok, record}], acc}

        %__MODULE__{} = batch_status, %__MODULE__{affected: 0} ->
          {[], batch_status}

        other, acc ->
          {[other], acc}
      end,
      fn
        %__MODULE__{} = batch_status -> {[batch_status], :already_emitted}
        affected -> {[%__MODULE__{inputs: inputs, affected: affected}], :new}
      end,
      & &1
    )
  end
end
