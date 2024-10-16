defmodule Ash.Domain.Verifiers.ValidateArgumentsToCodeInterface do
  @moduledoc """
  Validate the arguments defined in the code interface
  and reject arguments that are not action attributes/arguments
  """
  use Spark.Dsl.Verifier

  alias Ash.Resource.Verifiers.ValidateArgumentsToCodeInterface

  @impl true
  def verify(dsl) do
    for reference <- Ash.Domain.Info.resource_references(dsl) do
      for interface <- reference.definitions, match?(%Ash.Resource.Interface{}, interface) do
        resource = reference.resource

        action_name = interface.action || interface.name

        action = Ash.Resource.Info.action(reference.resource, action_name)

        resource_attributes = Ash.Resource.Info.attributes(reference.resource)

        ValidateArgumentsToCodeInterface.verify_interface!(
          interface,
          action,
          resource_attributes,
          resource
        )
      end
    end

    :ok
  end
end
