# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateTemporalKeys do
  @moduledoc """
  Verifies `temporal_keys` on relationships involving temporal resources.

  When either side of a `has_one`/`has_many`/`belongs_to` is temporal, the
  relationship must declare `temporal_keys` matching each side's period attribute
  (`nil` for a non-temporal side), and `has_one`/`has_many` must set
  `no_attributes? true` (a temporal destination is not uniquely keyed by the
  foreign key — the join is expressed via the filter).

  This runs as a verifier (not a transformer) so it can inspect the destination
  resource's temporality without introducing a compile-time dependency.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @types [:has_one, :has_many, :belongs_to]

  @impl true
  def verify(dsl_state) do
    module = Verifier.get_persisted(dsl_state, :module)
    source_temporal? = Ash.Resource.Info.temporal?(dsl_state)
    source_attribute = Ash.Resource.Info.temporal_attribute(dsl_state)

    dsl_state
    |> Verifier.get_entities([:relationships])
    |> Enum.filter(&(&1.type in @types))
    |> Enum.reduce_while(:ok, fn relationship, :ok ->
      case validate(relationship, module, source_temporal?, source_attribute) do
        :ok -> {:cont, :ok}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp validate(relationship, module, source_temporal?, source_attribute) do
    if Code.ensure_loaded?(relationship.destination) do
      dest_temporal? = Ash.Resource.Info.temporal?(relationship.destination)
      dest_attribute = Ash.Resource.Info.temporal_attribute(relationship.destination)

      if not source_temporal? and not dest_temporal? do
        :ok
      else
        expected =
          {if(source_temporal?, do: source_attribute), if(dest_temporal?, do: dest_attribute)}

        check(relationship, module, expected)
      end
    else
      :ok
    end
  end

  defp check(relationship, module, expected) do
    cond do
      relationship.temporal_keys != expected ->
        error(
          module,
          relationship,
          "involves a temporal resource and must set `temporal_keys #{inspect(expected)}` " <>
            "(got #{inspect(relationship.temporal_keys)})"
        )

      Map.has_key?(relationship, :no_attributes?) and not relationship.no_attributes? ->
        error(
          module,
          relationship,
          "involves a temporal resource and must set `no_attributes? true` " <>
            "(a temporal destination is not uniquely keyed by the foreign key)"
        )

      true ->
        :ok
    end
  end

  defp error(module, relationship, message) do
    {:error,
     DslError.exception(
       module: module,
       path: [:relationships, relationship.name],
       message: "Relationship `#{relationship.name}` #{message}"
     )}
  end
end
