# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.AddTemporalRelationshipFilters do
  @moduledoc """
  Temporal compile-time wiring:

  * Rejects accepting the period attribute as action input — a temporal write always
    establishes validity from `as_of` onward, so the period is set by the data layer,
    never by the caller.
  * Bakes a `range_overlaps(parent(source), destination)` filter into relationships
    that declare `temporal_keys` with both a source and destination period attribute.

  The relationship filter is done at transform time (not by checking the destination's
  temporality, which would introduce a compile-time dependency) — it reads only the
  declared `temporal_keys` option and attribute names. Because the overlap becomes part
  of `relationship.filter`, every consumer (in-memory loads and data-layer joins)
  applies it automatically. A separate verifier checks `temporal_keys` is correct.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError
  require Ash.Expr
  import Ash.Expr

  def transform(dsl_state) do
    :ok = verify_period_not_accepted!(dsl_state)

    dsl_state
    |> Ash.Resource.Info.relationships()
    |> Enum.reduce({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      case Map.get(relationship, :temporal_keys) do
        {source, destination} when not is_nil(source) and not is_nil(destination) ->
          overlap = expr(range_overlaps(parent(^ref(source)), ^ref(destination)))

          new_filter =
            case relationship.filter do
              nil -> overlap
              existing -> expr(^existing and ^overlap)
            end

          new_dsl_state =
            Transformer.replace_entity(
              dsl_state,
              [:relationships],
              %{relationship | filter: new_filter},
              &(&1.name == relationship.name)
            )

          {:ok, new_dsl_state}

        _ ->
          {:ok, dsl_state}
      end
    end)
  end

  # Raises (failing compilation) if the temporal period attribute is accepted as input.
  # Runs after `DefaultAccept` so `accept :*` is already resolved to a concrete list.
  defp verify_period_not_accepted!(dsl_state) do
    attribute = Transformer.get_option(dsl_state, [:temporal], :attribute)

    if is_nil(Transformer.get_option(dsl_state, [:temporal], :strategy)) do
      :ok
    else
      accepting =
        dsl_state
        |> Transformer.get_entities([:actions])
        |> Enum.filter(
          &(&1.type in [:create, :update] && attribute in (Map.get(&1, :accept) || []))
        )
        |> Enum.map(& &1.name)

      if accepting == [] do
        :ok
      else
        raise DslError.exception(
                module: Transformer.get_persisted(dsl_state, :module),
                path: [:temporal, :attribute],
                message:
                  "the temporal attribute #{inspect(attribute)} must not be accepted as input " <>
                    "(actions: #{inspect(accepting)}); set the period via `as_of` instead"
              )
      end
    end
  end

  # Run after the relationship source is set + accepts resolved, before filters are
  # cached/validated.
  def after?(Ash.Resource.Transformers.SetRelationshipSource), do: true
  def after?(Ash.Resource.Transformers.DefaultAccept), do: true
  def after?(_), do: false
end
