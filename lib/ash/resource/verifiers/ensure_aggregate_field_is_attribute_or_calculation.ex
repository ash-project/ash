defmodule Ash.Resource.Verifiers.EnsureAggregateFieldIsAttributeOrCalculation do
  @moduledoc """
  Ensures that the field at the end of the path is an attribute or calculation.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    relationships = Ash.Resource.Info.relationships(dsl)
    aggregates = Ash.Resource.Info.aggregates(dsl)

    for %{name: name, relationship_path: paths} <- aggregates do
      relationship_name = Enum.at(paths, -1)

      case Enum.find(relationships, &(&1.name == relationship_name)) do
        %{destination: destination} ->
          is_attribute? = Ash.Resource.Info.attribute(destination, name)

          is_calculation? = Ash.Resource.Info.calculation(destination, name)

          if !is_attribute? and !is_calculation? do
            raise Spark.Error.DslError,
              module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
              message:
                "All aggregates keys must be attributes or calculations. Got: #{inspect(name)}",
              path: [:aggregates, name]
          end
      end
    end

    :ok
  end
end
