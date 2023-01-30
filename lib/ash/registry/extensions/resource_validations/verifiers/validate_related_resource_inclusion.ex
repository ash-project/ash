defmodule Ash.Registry.ResourceValidations.Verifiers.ValidateRelatedResourceInclusion do
  @moduledoc """
  Ensures that all related resources are included in an API.
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    resources =
      dsl
      |> Verifier.get_entities([:entries])
      |> Enum.map(& &1.entry)

    for resource <- resources do
      for relationship <- Ash.Resource.Info.relationships(resource) do
        unless relationship.api || relationship.destination in resources do
          raise """
          Could not determine api for #{inspect(resource)}.#{relationship.name}. Please do one of the following:

          1. Add the resource to the registry `#{Verifier.get_persisted(dsl, :module)}`

          2. Set the `api` option on the relationship, for example:


          #{relationship.type} :#{relationship.name}, #{inspect(relationship.destination)} do
            api SomeOtherApi
          end
          """
        end
      end
    end

    :ok
  end
end
