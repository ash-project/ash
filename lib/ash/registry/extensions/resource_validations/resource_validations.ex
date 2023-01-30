defmodule Ash.Registry.ResourceValidations do
  @moduledoc """
  Adds some top level validations of resources present in a registry
  """

  @verifiers [
    Ash.Registry.ResourceValidations.Verifiers.EnsureResourcesCompiled,
    Ash.Registry.ResourceValidations.Verifiers.ValidateRelatedResourceInclusion,
    Ash.Registry.ResourceValidations.Verifiers.EnsureNoEmbeds
  ]

  use Spark.Dsl.Extension, verifiers: @verifiers
end
