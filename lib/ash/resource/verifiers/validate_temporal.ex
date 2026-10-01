# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateTemporal do
  @moduledoc """
  Ensures that the temporal configuration is valid for the given resource
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Extension
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    strategy = Verifier.get_option(dsl_state, [:temporal], :strategy)
    resource = Verifier.get_persisted(dsl_state, :module)
    data_layer = Verifier.get_persisted(dsl_state, :data_layer)

    cond do
      is_nil(strategy) ->
        :ok

      strategy == :context && data_layer &&
          not Ash.DataLayer.can?(data_layer, resource, :temporal) ->
        strategy_anno = Extension.get_opt_anno(dsl_state, [:temporal], :strategy)

        {:error,
         DslError.exception(
           module: resource,
           path: [:temporal, :strategy],
           message: "Data layer does not support temporal resources",
           location: strategy_anno
         )}

      true ->
        :ok
    end
  end
end
