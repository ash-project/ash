defmodule Ash.Registry.Dsl do
  @entry %Spark.Dsl.Entity{
    name: :entry,
    describe: "A reference to an ash module (typically a resource)",
    target: Ash.Registry.Entry,
    args: [:entry],
    links: [],
    examples: [
      "entry MyApp.User"
    ],
    schema: [
      entry: [
        type: :atom,
        required: true,
        doc: "The referenced module",
        links: []
      ]
    ]
  }

  @entries %Spark.Dsl.Section{
    name: :entries,
    describe: "List the entries present in this registry",
    links: [],
    examples: [
      """
      entries do
        entry MyApp.User
        entry MyApp.Post
        entry MyApp.Comment
      end
      """
    ],
    entities: [
      @entry
    ],
    schema: [
      warn_on_empty?: [
        type: :boolean,
        doc: "Set to `false` to ignore warnings about an empty registry",
        default: true,
        links: []
      ]
    ]
  }

  @sections [@entries]

  @transformers [Ash.Registry.Transformers.WarnOnEmpty]

  @moduledoc """
  A small DSL for declaring an `Ash.Registry`.

  `Ash.Registry` can be used generically, but the main way it is used in Ash is to provide a compile-time registry for an Ash Api.

  <!--- ash-hq-hide-start--> <!--- -->

  ## DSL Documentation

  ### Index

  #{Spark.Dsl.Extension.doc_index(@sections)}

  ### Docs

  #{Spark.Dsl.Extension.doc(@sections)}
  <!--- ash-hq-hide-stop--> <!--- -->
  """

  use Spark.Dsl.Extension, sections: @sections, transformers: @transformers
end
