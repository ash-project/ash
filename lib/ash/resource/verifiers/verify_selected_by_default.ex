defmodule Ash.Resource.Verifiers.VerifySelectedByDefault do
  @moduledoc """
  Raises an error when a required primary key is missing
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  def verify(dsl) do
    resource = Verifier.get_persisted(dsl, :module)
    data_layer = Ash.DataLayer.data_layer(resource)

    if is_nil(data_layer) || data_layer.can?(resource, :select) do
      :ok
    else
      Enum.each(Ash.Resource.Info.attributes(dsl), fn attribute ->
        if !attribute.select_by_default? do
          raise Spark.Error.DslError,
            module: resource,
            path: [:attributes, attribute.name],
            message: """
            Attribute #{inspect(resource)}.#{attribute.name} was marked with `select_by_default: false`,
            but the data layer #{inspect(data_layer)} does not support selecting attributes.

            This means that all attributes will always be selected.
            """
        end
      end)
    end
  end
end
