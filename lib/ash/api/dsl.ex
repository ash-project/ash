defmodule Ash.Api.Dsl do
  @resource %Ash.Dsl.Entity{
    name: :resource,
    describe: "A reference to a resource",
    target: Ash.Api.ResourceReference,
    args: [:resource],
    examples: [
      "resource MyApp.User"
    ],
    # This is an internal tool used by embedded resources,
    # so we hide it from the documentation
    hide: [:warn_on_compile_failure?],
    schema: [
      warn_on_compile_failure?: [
        type: :atom,
        default: true
      ],
      as: [
        type: :atom,
        required: false,
        doc: """
        A short name for the resource.

        Can be used in calls to Api modules, e.g `Api.read(:special_thing)`.
        """
      ],
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
    examples: [
      """
      resources do
        resource MyApp.User
        resource MyApp.Post
        resource MyApp.Comment
      end
      """
    ],
    entities: [
      @resource
    ]
  }

  @transformers [
                  EnsureResourcesCompiled,
                  ValidateRelatedResourceInclusion,
                  ValidateRelationshipAttributes,
                  ValidateManyToManyJoinAttributes
                ]
                |> Enum.map(&Module.concat(["Ash", Api, Transformers, &1]))

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
