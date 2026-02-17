# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Section do
  @moduledoc """
  Declares a DSL section.

  A dsl section allows you to organize related configurations. All extensions
  configure sections, they cannot add DSL builders to the top level. This
  keeps things organized, and concerns separated.

  A section may have nested sections, which will be configured the same as other sections.
  Getting the options/entities of a section is done by providing a path, so you would
  use the nested path to retrieve that configuration. See `Spark.Dsl.Extension.get_entities/2`
  and `Spark.Dsl.Extension.get_opt/4`.

  A section may have entities, which are constructors that produce instances of structs.
  For more on entities, see `Spark.Dsl.Entity`.

  A section may also have a `schema`, which you can learn more about in `Spark.Options`. Spark will produce
  builders for those options, so that they may be configured. They are retrieved with
  `Spark.Dsl.Extension.get_opt/4`.

  To create a section that is available at the top level (i.e not  nested inside of its own name), use
  `top_level?: true`. Remember, however, that this has no effect on sections nested inside of other sections.

  For a full example, see `Spark.Dsl.Extension`.
  """
  defstruct [
    :name,
    imports: [],
    schema: [],
    describe: "",
    snippet: "",
    links: nil,
    after_define: nil,
    examples: [],
    modules: [],
    top_level?: false,
    no_depend_modules: [],
    auto_set_fields: [],
    deprecations: [],
    entities: [],
    sections: [],
    docs: "",
    patchable?: false
  ]

  alias Spark.{
    Dsl.Entity,
    Dsl.Section
  }

  @type name :: atom()

  @type imports :: [module]

  @typedoc """
  User provided documentation.

  Documentation provided in a `Section`'s `describe` field will be included by `Spark` in any generated documentation that includes the `Section`.
  """
  @type describe :: String.t()

  @type snippet :: String.t()
  @typedoc """
  Determines whether a section can be declared directly in a module.

  When `top_level?: true`, that Section's DSL can be declared outside of a `do` block in a module.

  ## Example

  A `Section` declared with `top_level?: true`:

  ```elixir
  @my_section %Spark.Dsl.Section{
    top_level?: true,
    name: :my_section,
    schema: [my_field: [type: :atom, required: true]]
  }
  ```

  Can be declared like this:

  ```elixir
  defmodule MyDslModule do
    my_field :value
  end
  ```

  With `top_level?: false`, the DSL section would need to be declared explicitly/:

  ```elixir
  defmodule MyDslModule do
    my_section do
      my_field :value
    end
  end
  ```
  """
  @type top_level?() :: boolean()

  @type links :: nil | keyword([String.t()])

  @type examples() :: [String.t()]

  @type modules :: [atom]

  @type no_depend_modules() :: [atom]

  @type auto_set_fields() :: keyword(any)

  @type entities :: [Entity.t()]

  @type sections :: [Section.t()]

  @typedoc """
  Internal field. Not set by user.
  """
  @type docs :: String.t()

  @type patchable? :: boolean()

  @type t :: %Section{
          name: name(),
          imports: imports(),
          schema: Spark.Options.schema(),
          describe: describe(),
          snippet: snippet(),
          top_level?: top_level?(),
          links: links(),
          examples: examples(),
          modules: modules(),
          no_depend_modules: no_depend_modules(),
          auto_set_fields: auto_set_fields(),
          entities: entities(),
          sections: sections(),
          docs: docs(),
          patchable?: patchable?()
        }
end
