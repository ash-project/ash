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
    args: [],
    schema: [],
    auto_set_fields: []
  ]

  @type t :: %__MODULE__{
          name: atom,
          describe: String.t(),
          target: module,
          examples: [String.t()],
          transform: mfa | nil,
          args: [atom],
          entities: Keyword.t(),
          auto_set_fields: Keyword.t(),
          schema: NimbleOptions.schema()
        }

  def build(
        %{target: target, schema: schema, auto_set_fields: auto_set_fields, transform: transform},
        opts,
        nested_entities
      ) do
    with {:ok, opts} <- NimbleOptions.validate(opts, schema),
         opts <- Keyword.merge(opts, auto_set_fields || []),
         built <- struct(target, opts),
         built <- struct(built, nested_entities),
         {:ok, built} <-
           transform(transform, built) do
      {:ok, built}
    end
  end

  defp transform(nil, built), do: {:ok, built}

  defp transform({module, function, args}, built) do
    apply(module, function, [built | args])
  end

  def validate(%{target: target}, built) do
    target.validate(built)
  end

  def describe(entity, depth \\ 2) do
    args_description =
      case Keyword.take(entity.schema, entity.args) do
        [] ->
          ""

        args_schema ->
          args_schema =
            Enum.map(args_schema, fn {key, value} ->
              {key, Keyword.drop(value, [:required, :default])}
            end)

          "\n" <>
            header("Arguments", depth) <>
            NimbleOptions.docs(args_schema)
      end

    opts_description =
      case Keyword.drop(entity.schema, entity.args) do
        [] ->
          ""

        opts_schema ->
          "\n" <>
            header("Options", depth) <>
            NimbleOptions.docs(opts_schema)
      end

    example_docs =
      case entity.examples do
        [] ->
          ""

        examples ->
          "\n" <>
            header("Examples", depth) <>
            Enum.map_join(examples, "\n", fn example ->
              "```elixir\n" <> example <> "\n```\n"
            end)
      end

    entity.describe <> "\n" <> example_docs <> args_description <> opts_description
  end

  defp header(header, depth) do
    String.duplicate("#", depth) <> " " <> header <> "\n\n"
  end
end
