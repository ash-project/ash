defmodule Ash.Resource.Transformers.RequireAlphanumericFieldNames do
  @moduledoc """
  Confirms that a resource does not have unaccepted characters (characters other than alphanumeric and underscores) in field names.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])

    relationships =
      dsl_state
      |> Transformer.get_entities([:relationships])

    calculations =
      dsl_state
      |> Transformer.get_entities([:calculations])

    aggregates =
      dsl_state
      |> Transformer.get_entities([:aggregates])

    attributes
    |> Enum.concat(relationships)
    |> Enum.concat(calculations)
    |> Enum.concat(aggregates)
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, _groups} ->
      unless name_acceptable?(name) do
        raise DslError.exception(message: "The field name #{name} contains special characters")
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false

  defp name_acceptable?(name) do
    name
    |> Atom.to_string()
    |> String.match?(~r/^[[:alnum:]_]+$/)
  end
end
