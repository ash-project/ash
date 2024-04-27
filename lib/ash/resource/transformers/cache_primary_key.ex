defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.filter(& &1.primary_key?)

    with :ok <- validate_attributes_not_nil(attributes, dsl_state) do
      dsl_state =
        dsl_state
        |> Transformer.persist(:primary_key, Enum.map(attributes, & &1.name))
        |> Transformer.persist(
          :primary_key_simple_equality?,
          Enum.all?(attributes, &Ash.Type.simple_equality?(&1.type))
        )

      {:ok, dsl_state}
    end
  end

  defp validate_attributes_not_nil(attributes, dsl_state) do
    Enum.reduce_while(attributes, :ok, fn attribute, :ok ->
      case validate_attribute_not_nil(attribute, dsl_state) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp validate_attribute_not_nil(attribute, dsl_state) when attribute.allow_nil? do
    message = """
    Primary keys must not be allowed to be nil.

    The attribute `#{attribute.name}` is set as a primary key and allows nil values.
    """

    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:attributes],
       message: message
     )}
  end

  defp validate_attribute_not_nil(_, _), do: :ok

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true

  def after?(_), do: false
end
