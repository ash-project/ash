# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Domain.Transformers.DedupResources do
  @moduledoc false
  use Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl_state) do
    case Spark.Dsl.Transformer.get_entities(dsl_state, [:resources]) do
      [] ->
        {:ok, dsl_state}

      references ->
        deduped = dedup(references)
        {:ok, update_in(dsl_state, [[:resources], :entities], fn _ -> deduped end)}
    end
  end

  defp dedup(references) do
    {order, by_resource} =
      Enum.reduce(references, {[], %{}}, fn reference, {order, by_resource} ->
        case Map.fetch(by_resource, reference.resource) do
          {:ok, existing} ->
            merged = %{
              existing
              | definitions: existing.definitions ++ reference.definitions
            }

            {order, Map.put(by_resource, reference.resource, merged)}

          :error ->
            {[reference.resource | order], Map.put(by_resource, reference.resource, reference)}
        end
      end)

    order
    |> Enum.reverse()
    |> Enum.map(&Map.fetch!(by_resource, &1))
  end
end
