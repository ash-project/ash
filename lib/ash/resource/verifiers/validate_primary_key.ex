# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidatePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    primary_key = Verifier.get_persisted(dsl_state, :primary_key)

    case primary_key do
      [] ->
        :ok

      [_] ->
        :ok

      _ ->
        data_layer = Verifier.get_persisted(dsl_state, :data_layer)
        resource = Verifier.get_persisted(dsl_state, :module)

        if data_layer && data_layer.can?(resource, :composite_primary_key) do
          :ok
        else
          # Find the second primary key attribute (the one causing the composite key issue)
          second_pk_name = Enum.at(primary_key, 1)
          second_pk_attribute = Ash.Resource.Info.attribute(dsl_state, second_pk_name)
          location = Entity.anno(second_pk_attribute)

          {:error,
           DslError.exception(
             module: resource,
             path: [:attributes, second_pk_name],
             location: location,
             message: "Data layer does not support composite primary keys"
           )}
        end
    end
  end
end
