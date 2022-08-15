defmodule Ash.Registry.Dsl do
  @entry %Spark.Dsl.Entity{
    name: :entry,
    describe: "A reference to an ash module (typically a resource)",
    target: Ash.Registry.Entry,
    args: [:entry],
    examples: [
      "entry MyApp.User"
    ],
    schema: [
      entry: [
        type: :atom,
        required: true,
        doc: "The referenced module"
      ]
    ]
  }

  @entries %Spark.Dsl.Section{
    name: :entries,
    describe: "List the entries present in this registry",
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
        default: true
      ]
    ]
  }

  @sections [@entries]

  @transformers [Ash.Registry.Transformers.WarnOnEmpty]

  @moduledoc """
  A small DSL for declaring an `Ash.Registry`.
  """

  use Spark.Dsl.Extension, sections: @sections, transformers: @transformers
end
