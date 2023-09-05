defmodule Ash.Resource.Transformers.ValidateActionTypesSupported do
  @moduledoc """
  Confirms that all action types declared on a resource are supported by its data layer
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def after_compile?, do: true

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.reject(&(&1.type in [:read, :action]))
    |> Enum.each(fn action ->
      data_layer = Transformer.get_persisted(dsl_state, :data_layer)
      resource = Transformer.get_persisted(dsl_state, :module)

      unless data_layer && data_layer.can?(resource, action.type) do
        message = """
        `#{inspect(data_layer)}` does not support `#{action.type}` actions on this resource.

        This is either because of a limitation in the data layer, or specific configuration of the resource.
        """

        raise DslError,
          module: resource,
          message: message,
          path: [:actions, action.type, action.name]
      end
    end)

    {:ok, dsl_state}
  end
end
