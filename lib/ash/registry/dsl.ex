defmodule Ash.Registry.Dsl do
  @entry %Ash.Dsl.Entity{
    name: :entry,
    describe: "A reference to an ash module (typically a resource)",
    target: Ash.Registry.Entry,
    args: [:entry],
    examples: [
      "entry MyApp.User"
    ],
    modules: [
      :entry
    ],
    schema: [
      entry: [
        type: :atom,
        required: true,
        doc: "The referenced module"
      ]
    ]
  }

  @entries %Ash.Dsl.Section{
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
    ]
  }

  @sections [@entries]

  @moduledoc """
  A small DSL for declaring an `Ash.Registry`.

  # Table of Contents
  #{Ash.Dsl.Extension.doc_index(@sections)}

  #{Ash.Dsl.Extension.doc(@sections)}
  """

  use Ash.Dsl.Extension, sections: @sections
end
