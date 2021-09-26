defmodule Ash.Dsl.Entity do
  @moduledoc """
  Declares a DSL entity.

  A dsl entity represents a dsl constructor who's resulting value is a struct.
  This lets the user create complex objects with arbitrary(mostly) validation rules.

  The lifecycle of creating entities is complex, happening as Elixir is compiling
  the modules in question. Some of the patterns around validating/transforming entities
  have not yet solidified. If you aren't careful and don't follow the guidelines listed
  here, you can have subtle and strange bugs during compilation. Anything not isolated to
  simple value validations should be done in `transformers`. See `Ash.Dsl.Transformer`.

  An entity has a `target` indicating which struct will ultimately be built. An entity
  also has a `schema`. This schema is used for documentation, and the options are validated
  against it before continuing on with the DSL.

  To create positional arguments to the builder, use `args`. The values provided to
  `args` need to be in the provided schema as well. They will be positional arguments
  in the same order that they are provided in the `args` key.

  `auto_set_fields` will set the provided values into the produced struct (they do not need
  to be included in the schema).

  `transform` is a function that takes a created struct and can alter it. This happens immediately
  after handling the DSL options, and can be useful for setting field values on a struct based on
  other values in that struct. If you need things that aren't contained in that struct, use an
  `Ash.Dsl.Transformer`.

  `entities` allows you to specify a keyword list of nested entities. Nested entities are stored
  on the struct in the corresponding key, and are used in the same way entities are otherwise.

  For a full example, see `Ash.Dsl.Extension`.
  """
  defstruct [
    :name,
    :target,
    :transform,
    examples: [],
    entities: [],
    describe: "",
    snippet: "",
    args: [],
    hide: [],
    modules: [],
    schema: [],
    auto_set_fields: [],
    docs: ""
  ]

  def build(
        %{target: target, schema: schema, auto_set_fields: auto_set_fields, transform: transform},
        opts,
        nested_entities
      ) do
    {before_validate_auto, after_validate_auto} =
      Keyword.split(auto_set_fields || [], Keyword.keys(schema))

    with {:ok, opts} <-
           Ash.OptionsHelpers.validate(Keyword.merge(opts || [], before_validate_auto), schema),
         opts <- Keyword.merge(opts, after_validate_auto),
         opts <- Enum.map(opts, fn {key, value} -> {schema[key][:as] || key, value} end),
         built <- struct(target, opts),
         built <- struct(built, nested_entities),
         {:ok, built} <-
           transform(transform, built) do
      {:ok, built}
    end
  end

  def transform(nil, built), do: {:ok, built}

  def transform({module, function, args}, built) do
    apply(module, function, [built | args])
  end
end
