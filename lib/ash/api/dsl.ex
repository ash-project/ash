defmodule Ash.Api.Dsl do
  @resource %Ash.Dsl.Entity{
    name: :resource,
    describe: "A reference to a resource",
    target: Ash.Api.ResourceReference,
    modules: [:resource],
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
    examples: [
      """
      resources do
        resource MyApp.User
        resource MyApp.Post
        resource MyApp.Comment
      end
      """
    ],
    schema: [
      define_interfaces?: [
        type: :boolean,
        default: false,
        doc: """
        If set to true, the code interface of each resource will be defined in the api.

        Keep in mind that this can increase the compile times of your application.
        """
      ]
    ],
    entities: [
      @resource
    ]
  }

  @transformers [
    Ash.Api.Transformers.ValidateRelatedResourceInclusion,
    Ash.Api.Transformers.ValidateRelationshipAttributes,
    Ash.Api.Transformers.ValidateManyToManyJoinAttributes
  ]

  @sections [@resources]

  @moduledoc """
  A small DSL for declaring APIs

  Apis are the entrypoints for working with your resources.

  Apis may optionally include a list of resources, in which case they can be
  used as an `Ash.Registry` in various places. This is for backwards compatibility,
  but if at all possible you should define an `Ash.Registry` if you are using an extension
  that requires a list of resources. For example, most extensions look for two application
  environment variables called `:ash_apis` and `:ash_registries` to find any potential registries

  # Table of Contents
  #{Ash.Dsl.Extension.doc_index(@sections)}

  #{Ash.Dsl.Extension.doc(@sections)}
  """

  use Ash.Dsl.Extension, sections: @sections, transformers: @transformers
end
