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
    modules: [],
    entities: [],
    sections: []
  ]

  @type t :: %__MODULE__{
          name: atom,
          describe: String.t(),
          entities: [Ash.Dsl.Entity.t()],
          sections: [%__MODULE__{}],
          schema: NimbleOptions.schema()
        }

  def describe(mod, section, depth \\ 2) do
    options_doc =
      if section.schema && section.schema != [] do
        "\n" <> header("Options", depth) <> "\n" <> NimbleOptions.docs(section.schema)
      else
        ""
      end

    entity_doc =
      case section.entities do
        [] ->
          ""

        entities ->
          "\n" <>
            header("Constructors", depth) <>
            Enum.map_join(entities, "\n", fn entity ->
              nested_module_name = Module.concat(mod, Macro.camelize(to_string(entity.name)))

              "* " <> to_string(entity.name) <> " - " <> "`#{inspect(nested_module_name)}`"
            end)
      end

    section_doc =
      case section.sections do
        [] ->
          ""

        sections ->
          "\n" <>
            header("Sections", depth) <>
            Enum.map_join(sections, "\n", fn section ->
              nested_module_name = Module.concat(mod, Macro.camelize(to_string(section.name)))
              "* " <> to_string(section.name) <> " - " <> "`#{inspect(nested_module_name)}`"
            end)
      end

    section.describe <> options_doc <> entity_doc <> section_doc
  end

  defp header(header, depth) do
    String.duplicate("#", depth) <> " " <> header <> "\n\n"
  end
end
