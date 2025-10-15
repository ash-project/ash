# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Extension
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  # sobelow_skip ["DOS.BinToAtom"]
  @impl true
  def verify(dsl_state) do
    strategy = Verifier.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Verifier.get_option(dsl_state, [:multitenancy], :attribute)
    attributes = Verifier.get_entities(dsl_state, [:attributes])
    resource = Verifier.get_persisted(dsl_state, :module)
    data_layer = Verifier.get_persisted(dsl_state, :data_layer)

    cond do
      strategy == :context && not data_layer.can?(resource, :multitenancy) ->
        # Get location info for the multitenancy strategy option
        strategy_anno = Extension.get_opt_anno(dsl_state, [:multitenancy], :strategy)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :strategy],
           message: "Data layer does not support multitenancy",
           location: strategy_anno
         )}

      strategy == :attribute && !Enum.any?(attributes, &(&1.name == attribute)) ->
        # Get location info for the multitenancy attribute option
        attribute_anno = Extension.get_opt_anno(dsl_state, [:multitenancy], :attribute)

        {:error,
         DslError.exception(
           module: resource,
           path: [:multitenancy, :attribute],
           message: "Attribute #{attribute} used in multitenancy configuration does not exist",
           location: attribute_anno
         )}

      true ->
        :ok
    end
  end
end
