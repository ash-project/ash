defmodule Ash.Resource.Transformers.RequireUniqueFieldNames do
  @moduledoc """
  Confirms that a resource does not have multiple fields(attributes, calculations, aggregates, and relationships) with the same name.
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
    |> Enum.each(fn {name, groups} ->
      name_count = Enum.count(groups)

      unless name_count == 1 do
        raise DslError.exception(
                message: """
                There are #{name_count} fields(attributes, calculations, aggregates, and relationships) that share the name `#{name}`
                """
              )
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false
end
