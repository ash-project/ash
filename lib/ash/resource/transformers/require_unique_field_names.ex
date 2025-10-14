# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Transformers.RequireUniqueFieldNames do
  @moduledoc """
  Confirms that a resource does not have multiple fields(attributes, calculations, aggregates, and relationships) with the same name.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    attributes = Ash.Resource.Info.attributes(dsl_state)
    relationships = Ash.Resource.Info.relationships(dsl_state)
    calculations = Ash.Resource.Info.calculations(dsl_state)
    aggregates = Ash.Resource.Info.aggregates(dsl_state)

    attributes
    |> Enum.concat(relationships)
    |> Enum.concat(calculations)
    |> Enum.concat(aggregates)
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, groups} ->
      name_count = Enum.count(groups)

      if name_count != 1 do
        # Find the second occurrence of this field name and get location of its name property
        second_field = Enum.at(groups, 1)

        location =
          case Entity.property_anno(second_field, :name) do
            nil -> Entity.anno(second_field)
            other -> other
          end

        raise DslError.exception(
                module: Transformer.get_persisted(dsl_state, :module),
                location: location,
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
