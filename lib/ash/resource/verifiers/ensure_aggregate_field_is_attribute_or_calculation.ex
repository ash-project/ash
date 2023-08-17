defmodule Ash.Resource.Verifiers.EnsureAggregateFieldIsAttributeOrCalculation do
  @moduledoc """
  Ensures that the field at the end of the path is an attribute or calculation.
  """
  use Spark.Dsl.Verifier

  def verify(dsl) do
    aggregates = Ash.Resource.Info.aggregates(dsl)

    for %{field: field, relationship_path: paths} <- aggregates do
      if is_nil(field) do
        :ok
      else
        case Ash.Resource.Info.related(dsl, paths) do
          nil ->
            :ok

          destination ->
            attribute = Ash.Resource.Info.attribute(destination, field)
            calculation = Ash.Resource.Info.calculation(destination, field)

            if !attribute and !calculation do
              raise Spark.Error.DslError,
                module: Spark.Dsl.Verifier.get_persisted(dsl, :module),
                message:
                  "All aggregates fields must be attributes or calculations. Got: #{inspect(field)}",
                path: [:aggregates, field]
            end
        end
      end
    end

    :ok
  end
end
