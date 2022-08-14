defmodule Ash.Registry.ResourceValidations do
  @moduledoc """
  Adds some top level validations of resources present in a registry
  """
  @transformers [
    Ash.Registry.ResourceValidations.Transformers.EnsureResourcesCompiled,
    Ash.Registry.ResourceValidations.Transformers.ValidateRelatedResourceInclusion,
    Ash.Registry.ResourceValidations.Transformers.EnsureNoEmbeds
  ]

  use Spark.Dsl.Extension, transformers: @transformers
end
