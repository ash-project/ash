# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateAtomicValidationDefaultTargetAttribute do
  @moduledoc """
  Ensures that the `atomic_validation_default_target_attribute` option references an existing attribute.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Extension
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    case Verifier.get_option(
           dsl_state,
           [:resource],
           :atomic_validation_default_target_attribute
         ) do
      nil ->
        :ok

      attribute ->
        attributes = Verifier.get_entities(dsl_state, [:attributes])

        if Enum.any?(attributes, &(&1.name == attribute)) do
          :ok
        else
          attribute_anno =
            Extension.get_opt_anno(
              dsl_state,
              [:resource],
              :atomic_validation_default_target_attribute
            )

          {:error,
           DslError.exception(
             module: Verifier.get_persisted(dsl_state, :module),
             path: [:resource, :atomic_validation_default_target_attribute],
             message:
               "Attribute #{inspect(attribute)} used for `atomic_validation_default_target_attribute` does not exist.",
             location: attribute_anno
           )}
        end
    end
  end
end
