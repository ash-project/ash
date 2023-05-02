defmodule Ash.Actions.BulkHelpers do
  # def halt_if(stream, condition, result) do
  #   Stream.transform(
  #     stream,
  #     fn -> :__halt_if_accumulator__ end,
  #     fn item, :__halt_if_accumulator__ ->
  #       if condition.(item) do
  #         {:halt, result.(item)}
  #       else
  #         {[item], :__halt_if_accumulator__}
  #       end
  #     end,
  #     fn
  #       :__halt_if_accumulator__ ->
  #         []

  #       result ->
  #         [result]
  #     end,
  #     fn _ -> :ok end
  #   )
  # end

  #   def in_batches(changeset_stream, batch_size, stop_on_errors?, per_batch) do
  #     changeset_stream
  #     |> Stream.chunk_every(batch_size)
  #     |> Stream.with_index(1)
  #     |> Stream.map(fn {batch, batch_count} ->
  #       case per_batch.(batch, batch_count) do
  #         case per_batch.(batch, batch_count) do
  #           {:ok, }
  #         end

  #       end
  #     end)
  #     Enum.reduce_while(
  #       changeset_stream,
  #       {:ok, {0, 0, [], [], [], []}},
  #       fn
  #         changeset,
  #         {:ok, {^batch_size, batch_count, batch, processed_results, errors, notifications}} ->
  #           batch = [changeset | batch]

  #           case per_batch.(batch, batch_count) do
  #             {:ok, batch_result, new_errors} ->
  #               if new_errors != [] and stop_on_errors? do
  #                 {:halt, {:ok, batch_result, new_errors, notifications}}
  #               else
  #                 {:cont,
  #                  {:ok,
  #                   {0, batch_count + 1, [], batch_result ++ processed_results,
  #                    errors ++ new_errors}}}
  #               end

  #             {:error, error} ->
  #               {:halt, {:error, error}}
  #           end

  #         changeset, {:ok, {i, batch_count, batch, processed_results, errors, notifications}} ->
  #           {:cont,
  #            {:ok,
  #             {i + 1, batch_count, [changeset | batch], processed_results, errors, notifications}}}
  #       end
  #     )
  #     |> case do
  #       {:ok, {_batch_size, _batch_count, [], processed_results, errors, notifications}} ->
  #         {:ok, processed_results, errors, notifications}

  #       {:ok, {_batch_size, batch_count, remaining_batch, processed_results, errors, notifications}} ->
  #         if errors != [] and stop_on_errors? do
  #           {:ok, processed_results, errors, notifications}
  #         else
  #           case per_batch.(remaining_batch, batch_count) do
  #             {:ok, batch_result, new_errors} ->
  #               {:ok, batch_result ++ processed_results, errors ++ new_errors, notifications}

  #             {:error, error} ->
  #               {:error, error}
  #           end
  #         end
  #     end
  #   end
end
