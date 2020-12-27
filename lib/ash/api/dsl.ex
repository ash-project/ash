defmodule Ash.Api.Dsl do
  @resource %Ash.Dsl.Entity{
    name: :resource,
    describe: "A reference to a resource",
    target: Ash.Api.ResourceReference,
    args: [:resource],
    examples: [
      "resource MyApp.User"
    ],
    schema: [
      resource: [
        type: :atom,
        required: true,
        doc: "The module of the resource"
      ]
    ]
  }

  @resources %Ash.Dsl.Section{
    name: :resources,
    describe: "List the resources present in this API",
    entities: [
      @resource
    ]
  }

  @transformers [
    Ash.Api.Transformers.EnsureResourcesCompiled,
    Ash.Api.Transformers.ValidateRelatedResourceInclusion,
    Ash.Api.Transformers.ValidateRelationshipAttributes,
    Ash.Api.Transformers.ValidateManyToManyJoinAttributes
  ]

  @sections [@resources]

  @moduledoc """
  A small DSL for declaring APIs

  Apis are the entrypoints for working with your resources.

  # Table of Contents
  #{Ash.Dsl.Extension.doc_index(@sections)}

  #{Ash.Dsl.Extension.doc(@sections)}
  """

  use Ash.Dsl.Extension, sections: @sections, transformers: @transformers
end
