# defmodule Ash.Registry.Dsl do
#   @entry %Ash.Dsl.Entity{
#     name: :entry,
#     describe: "A reference to a a module",
#     target: Ash.Registry.EntryReference,
#     args: [:entry],
#     examples: [
#       "entry MyApp.Post"
#     ],
#     schema: [
#       entry: [
#         type: :atom,
#         required: true,
#         doc: "The module of the entry"
#       ]
#     ]
#   }

#   @entries %Ash.Dsl.Section{
#     name: :entries,
#     describe: "List the entries present in this registry",
#     examples: [
#       """
#       entries do
#         entry MyApp.User
#         entry MyApp.Post
#         entry MyApp.Comment
#       end
#       """
#     ],
#     entities: [
#       @entry
#     ]
#   }

#   @sections [@resources]

#   @moduledoc """
#   A small DSL for declaring APIs

#   Apis are the entrypoints for working with your resources.

#   Apis may optionally include a list of resources, in which case they can be
#   used as an `Ash.Registry` in various places. This is for backwards compatibility,
#   but if at all possible you should define an `Ash.Registry` if you are using an extension
#   that requires a list of resources. For example, most extensions look for two application
#   environment variables called `:ash_apis` and `:ash_registries` to find any potential registries

#   # Table of Contents
#   #{Ash.Dsl.Extension.doc_index(@sections)}

#   #{Ash.Dsl.Extension.doc(@sections)}
#   """

#   use Ash.Dsl.Extension, sections: @sections, transformers: @transformers
# end
