defmodule Ash.Resource.Verifiers.EnsureAggregateFieldIsAttributeOrCalculation do
  @moduledoc """
  Ensures that the field at the end of the path is an attribute or calculation.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    aggregates = Ash.Resource.Info.aggregates(dsl)

    for %{name: name, relationship_path: paths} <- aggregates do
      case Ash.Resource.Info.related(dsl, paths) do
        nil ->
          nil

        destination ->
          attribute = Ash.Resource.Info.attribute(destination, name)
          calculation = Ash.Resource.Info.calculation(destination, name)

          if !attribute and !calculation do
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
