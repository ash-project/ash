# SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Spark.Dsl.Entity do
  @moduledoc """
  Declares a DSL entity.

  A dsl entity represents a dsl constructor who's resulting value is a struct.
  This lets the user create complex objects with arbitrary(mostly) validation rules.

  The lifecycle of creating entities is complex, happening as Elixir is compiling
  the modules in question. Some of the patterns around validating/transforming entities
  have not yet solidified. If you aren't careful and don't follow the guidelines listed
  here, you can have subtle and strange bugs during compilation. Anything not isolated to
  simple value validations should be done in `transformers`. See `Spark.Dsl.Transformer`.

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
  `Spark.Dsl.Transformer`. This function returns `{:ok, new_entity}` or `{:error, error}`, so this can
  also be used to validate the entity.

  `entities` allows you to specify a keyword list of nested entities. Nested entities are stored
  on the struct in the corresponding key, and are used in the same way entities are otherwise.

  `singleton_entity_keys` specifies a set of entity keys (specified above) that should only have a
  single value. This will be validated and unwrapped into `nil` | `single_value` on success.

  `identifier` expresses that a given entity is unique by that field, validated by the DSL.

  ## Example

  ```elixir
  @my_entity %Spark.Dsl.Entity{
    name: :my_entity,
    target: MyStruct,
    schema: [my_field: [type: :atom, required: false]]
  }
  ```

  Once compiled by Spark, entities can be invoked with a keyword list:

  ```elixir
  my_entity my_field: :value
  ```

  Or with a do block:

  ```elixir
  my_entity do
    my_field :value
  end
  ```

  For a full example, see `Spark.Dsl.Extension`.
  """

  defstruct [
    :name,
    :target,
    :transform,
    :recursive_as,
    examples: [],
    entities: [],
    singleton_entity_keys: [],
    deprecations: [],
    describe: "",
    snippet: "",
    args: [],
    links: nil,
    hide: [],
    identifier: nil,
    modules: [],
    imports: [],
    no_depend_modules: [],
    schema: [],
    auto_set_fields: [],
    docs: ""
  ]

  alias Spark.{Dsl.Entity}

  @typedoc """
  Defines the struct that will be built from this entity definition.

  The struct will need to have fields for all [`entities`](#t:entities/0), `t:schema/0` fields, and `t:auto_set_fields/0`.
  """
  @type target :: module() | nil

  @typedoc """
  A keyword list of nested entities.
  """
  @type entities :: keyword(t)

  @typedoc """
  Specifies a function that will run on the target struct after building.

  ```elixir
  @my_entity %Spark.Dsl.Entity{
    name: :my_entity,
    target: MyEntity,
    schema: [
      my_field: [type: :list, required: true]
    ],
    transform: {MyModule, :max_three_items, []}
  }

  def max_three_items(my_entity) do
    if length(my_entity.my_field) > 3 do
      {:error, "Can't have more than three items"}
    else
      {:ok, my_entity}
    end
  end
  ```
  """
  @type transform :: {module(), function :: atom(), args :: [any()]} | nil

  @typedoc """
  Specifies positional arguments for an Entity.

  An entity declared like this:

  ```elixir
  @entity %Spark.Dsl.Entity{
    name: :entity,
    target: Entity,
    schema: [
      positional: [type: :atom, required: true],
      other: [type: :atom, required: false],
    ],
    args: [:positional]
  }
  ```

  Can be instantiated like this:

  ```elixir
  entity :positional_argument do
    other :other_argument
  end
  ```
  """
  @type args :: [atom | {:optional, atom} | {:optional, atom, any}]

  @typedoc """
  Set the provided key value pairs in the produced struct. These fields do not need to be included in the Entity's schema.
  """
  @type auto_set_fields :: keyword(any)

  @type deprecations :: keyword(String.t())

  # Using type id() since identifier is a reserved type.
  @type id :: term()

  @type imports :: [module()]

  @type name :: atom | nil

  @typedoc """
  Internal field. Not set by user.
  """
  @type docs :: String.t()
  @typedoc """
  User provided documentation.

  Documentation provided in a `Entity`'s `describe` field will be included by `Spark` in any generated documentation that includes the `Entity`.
  """
  @type describe :: String.t()

  @type examples :: [String.t()]

  @type hide :: [atom()]

  @type links :: keyword([String.t()]) | nil

  @type modules :: [atom]

  @type no_depend_modules :: [atom]

  @type recursive_as :: atom | nil

  @type singleton_entity_keys :: [atom]

  @type snippet :: String.t()

  @type t :: %Entity{
          args: args(),
          auto_set_fields: auto_set_fields(),
          deprecations: deprecations(),
          describe: describe(),
          docs: docs(),
          entities: entities(),
          examples: examples(),
          hide: hide(),
          identifier: id(),
          imports: imports(),
          links: links(),
          modules: modules(),
          name: name(),
          no_depend_modules: no_depend_modules(),
          recursive_as: recursive_as(),
          schema: Spark.Options.schema(),
          singleton_entity_keys: singleton_entity_keys(),
          snippet: snippet(),
          target: target(),
          transform: transform()
        }

  @opaque spark_meta() :: %Spark.Dsl.Entity.Meta{
            anno: :erl_anno.anno() | nil,
            properties_anno: %{optional(atom()) => :erl_anno.anno() | nil}
          }

  @type entity() :: %{
          required(:__struct__) => module(),
          required(:__spark_metadata__) => spark_meta() | nil,
          optional(:__identifier__) => term(),
          optional(atom()) => term()
        }

  @doc false
  def arg_names(entity) do
    entity.args
    |> Kernel.||([])
    |> Enum.map(fn
      tuple when is_tuple(tuple) ->
        elem(tuple, 1)

      other ->
        other
    end)
  end

  @doc false
  def required_arg_names(entity) do
    entity.args
    |> Kernel.||([])
    |> Enum.filter(&is_atom/1)
  end

  @doc false
  def build(
        %{
          target: target,
          schema: schema,
          auto_set_fields: auto_set_fields,
          transform: transform,
          identifier: identifier,
          singleton_entity_keys: singleton_entity_keys,
          entities: nested_entity_definitions
        },
        opts,
        nested_entities,
        anno,
        opts_anno
      ) do
    with {:ok, opts, more_nested_entities} <-
           fetch_single_argument_entities_from_opts(
             opts,
             nested_entity_definitions,
             anno,
             opts_anno
           ),
         opts <- Keyword.new(opts),
         {before_validate_auto, after_validate_auto} =
           Keyword.split(auto_set_fields || [], Keyword.keys(schema)),
         {:ok, opts} <- Spark.Options.validate(Keyword.merge(opts, before_validate_auto), schema),
         opts <- Keyword.merge(opts, after_validate_auto),
         opts <- Enum.map(opts, fn {key, value} -> {schema[key][:as] || key, value} end),
         built <- struct(target, opts),
         built <-
           struct(
             built,
             Keyword.merge(
               Keyword.new(nested_entities),
               Keyword.new(more_nested_entities),
               fn _k, v1, v2 ->
                 v1 ++ v2
               end
             )
           ),
         {:ok, built} <- validate_singleton_entity_keys(built, singleton_entity_keys),
         built <- maybe_set_spark_metadata(built, anno, opts_anno || %{}),
         {:ok, built} <- transform(transform, built) do
      maybe_apply_identifier(built, identifier)
    end
  end

  defp maybe_set_spark_metadata(built, anno, opts_anno) do
    if Map.has_key?(built, :__spark_metadata__) do
      metadata = %Spark.Dsl.Entity.Meta{anno: anno, properties_anno: opts_anno}
      %{built | __spark_metadata__: metadata}
    else
      built
    end
  end

  defp fetch_single_argument_entities_from_opts(opts, nested_entity_definitions, anno, opts_anno) do
    Enum.reduce_while(nested_entity_definitions, {:ok, opts, []}, fn
      {key, entity_definitions}, {:ok, opts, more_nested_entities} ->
        entity_definitions
        |> Enum.filter(fn entity_definition -> Enum.count(entity_definition.args) == 1 end)
        |> Enum.reduce_while(
          {:ok, opts, more_nested_entities},
          fn entity_definition, {:ok, opts, more_nested_entities} ->
            values = Keyword.get_values(opts, entity_definition.name)
            opts = Keyword.delete(opts, entity_definition.name)

            Enum.reduce_while(values, {:ok, opts, more_nested_entities}, fn
              single_arg, {:ok, opts, more_nested_entities} ->
                case build(
                       entity_definition,
                       [{Enum.at(entity_definition.args, 0), single_arg}],
                       [],
                       anno,
                       opts_anno
                     ) do
                  {:ok, built} ->
                    {:cont,
                     {:ok, opts,
                      Keyword.update(
                        more_nested_entities,
                        key,
                        [built],
                        &[built | &1]
                      )}}

                  {:error, error} ->
                    {:halt, {:error, error}}
                end
            end)
            |> case do
              {:ok, opts, more_nested_entities} ->
                {:cont, {:ok, opts, more_nested_entities}}

              {:error, error} ->
                {:halt, {:error, error}}
            end
          end
        )
        |> case do
          {:ok, opts, more_nested_entities} ->
            {:cont, {:ok, opts, more_nested_entities}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
  end

  defp validate_singleton_entity_keys(entity, []), do: {:ok, entity}

  defp validate_singleton_entity_keys(entity, [key | rest]) do
    case Map.get(entity, key) do
      nil ->
        validate_singleton_entity_keys(entity, rest)

      [] ->
        validate_singleton_entity_keys(Map.put(entity, key, nil), rest)

      [nested_entity] ->
        validate_singleton_entity_keys(Map.put(entity, key, nested_entity), rest)

      entities ->
        {:error, "Expected a single #{key}, got #{Enum.count(entities)}"}
    end
  end

  @doc false
  def maybe_apply_identifier(struct, nil), do: {:ok, struct}

  def maybe_apply_identifier(struct, {:auto, :unique_integer})
      when is_map_key(struct, :__identifier__),
      do: {:ok, %{struct | __identifier__: System.unique_integer()}}

  def maybe_apply_identifier(struct, {:auto, :unique_integer}),
    do: raise("#{inspect(struct.__struct__)} must have the `__identifier__` field!")

  def maybe_apply_identifier(struct, name)
      when is_map_key(struct, :__identifier__) and is_map_key(struct, name),
      do: {:ok, %{struct | __identifier__: Map.get(struct, name)}}

  def maybe_apply_identifier(struct, name) when is_map_key(struct, :__identifier__),
    do: raise("#{inspect(struct.__struct__)} does not have a field named `#{inspect(name)}`!")

  def maybe_apply_identifier(struct, _name),
    do: raise("#{inspect(struct.__struct__)} must have the `__identifier__` field!")

  @doc false
  def transform(nil, built), do: {:ok, built}

  def transform({module, function, args}, built) do
    apply(module, function, [built | args])
  end

  @doc """
  Get the annotation for an entity.

  Returns the annotation that was captured when the entity was defined,
  or `nil` if no annotation is available or the entity doesn't have the
  `:__spark_metadata__` field.
  """
  @spec anno(entity()) :: :erl_anno.anno() | nil
  def anno(entity)

  def anno(%_{__spark_metadata__: %Spark.Dsl.Entity.Meta{anno: anno}}) when anno != nil,
    do: anno

  def anno(%_{__spark_metadata__: _}), do: nil

  def anno(%struct{}) do
    IO.warn(
      "Entity #{inspect(struct)} does not define a `__spark_metadata__` field. " <>
        "This field is required to access source annotations. " <>
        "Add `__spark_metadata__: nil` to the defstruct for #{inspect(struct)}."
    )

    nil
  end

  @doc """
  Get the annotation for a specific property of an entity.

  Returns the annotation that was captured when the property was set on the entity,
  or `nil` if no annotation is available for that property or the entity doesn't have
  the `:__spark_metadata__` field.
  """
  @spec property_anno(entity(), atom()) :: :erl_anno.anno() | nil
  def property_anno(entity, property)

  def property_anno(
        %_{__spark_metadata__: %Spark.Dsl.Entity.Meta{properties_anno: props}},
        property
      )
      when is_atom(property) do
    Map.get(props, property)
  end

  def property_anno(%_{__spark_metadata__: _}, _property), do: nil

  def property_anno(%struct{}, _property) do
    IO.warn(
      "Entity #{inspect(struct)} does not define a `__spark_metadata__` field. " <>
        "This field is required to access source annotations. " <>
        "Add `:__spark_metadata__` to the defstruct for #{inspect(struct)}."
    )

    nil
  end
end
