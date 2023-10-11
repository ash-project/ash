defmodule Ash.Resource.Verifiers.ValidatePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Verifier

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
          {:error,
           DslError.exception(
             module: resource,
             message: "Data layer does not support composite primary keys"
           )}
        end
    end
  end
end
