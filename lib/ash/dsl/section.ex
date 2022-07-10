defmodule Ash.Dsl.Section do
  @moduledoc """
  Declares a DSL section.

  A dsl section allows you to organize related configurations. All extensions
  configure sections, they cannot add DSL builders to the top level. This
  keeps things organized, and concerns separated.

  A section may have nested sections, which will be configured the same as other sections.
  Getting the options/entities of a section is done by providing a path, so you would
  use the nested path to retrieve that configuration. See `Ash.Dsl.Extension.get_entities/2`
  and `Ash.Dsl.Extension.get_opt/4`.

  A section may have entities, which are constructors that produce instances of structs.
  For more on entities, see `Ash.Dsl.Entity`.

  A section may also have a `schema`, which is a `NimbleOptions` schema. Ash will produce
  builders for those options, so that they may be configured. They are retrived with
  `Ash.Dsl.Extension.get_opt/4`.

  For a full example, see `Ash.Dsl.Extension`.
  """
  defstruct [
    :name,
    imports: [],
    schema: [],
    describe: "",
    snippet: "",
    links: [],
    examples: [],
    modules: [],
    no_depend_modules: [],
    auto_set_fields: [],
    deprecations: [],
    entities: [],
    sections: [],
    docs: ""
  ]
end
