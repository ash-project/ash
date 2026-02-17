<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Using Source Annotations

Spark automatically tracks source location information for all DSL elements
using Erlang's `:erl_anno` module. This provides comprehensive location tracking
for sections, options, and entities, enabling better error messages, IDE
integration, and debugging capabilities.

Source annotations are only enabled when the Elixir compile option `debug_info`
is enabled (`Code.get_compiler_option(:debug_info)` returns true). By default,
debug info is disabled in production and in `.exs` script files, which means
source annotations won't be available in those contexts.

> #### ExUnit Test Cases {: .warning}
>
> If you're defining modules inside ExUnit test cases (which use `.exs` files),
> source annotations won't be available unless you explicitly enable
> `debug_info` in your tests.
>
> ```elixir
> setup do
>   debug_info? = Code.get_compiler_option(:debug_info)
>   Code.put_compiler_option(:debug_info, true)
>   on_exit(fn -> Code.put_compiler_option(:debug_info, debug_info?) end)
>   :ok
> end
> ```

## What are Source Annotations?

Source annotations capture metadata about where DSL elements are defined in your
source code, including:

- **File path**: The source file where the DSL element is declared
- **Line number**: The exact line where the element starts
- **End location**: The line where DSL blocks end (available on OTP 28+,
  requires Elixir Parser Configuration)

```elixir
defmodule Acme.MixProject do
  use Mix.Project

  def project do
    [
      app: :acme,
      elixirc_options: [
        parser_options: [
          token_metadata: true,
          parser_columns: true
        ]
      ],
      # ...
    ]
  end
end
```

Spark tracks annotations for:
- **Sections**: Location where section blocks are defined (`section do ... end`)
- **Options**: Location where individual options are set (`option_name "value"`)
- **Entities**: Location where entities are declared (`entity :name do ... end`)

## Annotation Introspection

### Universal Access via Introspection Functions

Spark provides introspection functions that work regardless of whether entities
define an `anno_field`. These functions access annotation data stored in the DSL
state:

```elixir
# Get DSL state
dsl_state = MyModule.spark_dsl_config()

# Section annotations
section_anno = Spark.Dsl.Extension.get_section_anno(dsl_state, [:my_section])
if section_anno do
  # Extract line number (Spark currently provides line numbers only)
  line = case :erl_anno.location(section_anno) do
    {line_num, _col} -> line_num
    line_num -> line_num
  end
  file = :erl_anno.file(section_anno) |> to_string()
  IO.puts("Section defined at #{file}:#{line}")
end

# Option annotations
option_anno = Spark.Dsl.Extension.get_opt_anno(dsl_state, [:my_section], :option_name)
if option_anno do
  line = :erl_anno.location(option_anno)
  file = :erl_anno.file(option_anno) |> to_string()
  IO.puts("Option defined at #{file}:#{line}")
end

# Entity annotations
entities = Spark.Dsl.Extension.get_entities(dsl_state, [:my_section])
Enum.each(entities, fn entity ->
  case Spark.Dsl.Entity.anno(entity) do
    nil -> :ok
    anno ->
      line = :erl_anno.location(anno)
      file = :erl_anno.file(anno) |> to_string()
      IO.puts("Entity defined at #{file}:#{line}")
  end
end)
```

### Entity Annotations

For direct access to annotations, entities should include the
`__spark_metadata__` field in their struct definition:

```elixir
defmodule MyEntity do
  defstruct [
    :name,
    :__spark_metadata__ # Required for annotation access
  ]
end

@my_entity %Spark.Dsl.Entity{
  name: :my_entity,
  target: MyEntity,
  schema: [
    name: [type: :atom, required: true]
  ]
}

# Access annotations
entities = Spark.Dsl.Extension.get_entities(dsl_state, [:my_section])
Enum.each(entities, fn entity ->
  if entity_anno = Spark.Dsl.Entity.anno(entity) do
    line = :erl_anno.location(entity_anno)
    file = :erl_anno.file(entity_anno) |> to_string()
    IO.puts("Entity defined at #{file}:#{line}")
  end

  if name_anno = Spark.Dsl.Entity.property_anno(entity, :name) do
    line = :erl_anno.location(name_anno)
    file = :erl_anno.file(name_anno) |> to_string()
    IO.puts("Entity name property defined at #{file}:#{line}")
  end
end)
```

## Working with Annotations

Annotations use Erlang's `:erl_anno` module, which provides several utilities:

```elixir
# Check if something is an annotation
:erl_anno.is_anno(anno)

# Get the location (line number or {line, column})
# Note: Spark currently only provides line numbers, not column information
location = :erl_anno.location(anno)

# Helper function to extract line number from location
get_line = fn location ->
  case location do
    {line_num, _column} -> line_num  # Future column support
    line_num when is_integer(line_num) -> line_num  # Current Spark behavior
  end
end

line = get_line.(location)

# Get the file (returns :undefined or a charlist)
file = :erl_anno.file(anno)

# Convert charlist to string safely
file_string = case file do
  :undefined -> "unknown"
  charlist -> to_string(charlist)
end

# Get the end location (OTP 28+, returns :undefined if not available)
if function_exported?(:erl_anno, :end_location, 1) do
  end_location = :erl_anno.end_location(anno)
end
```

## Use Cases

### Enhanced Error Messages in Verifiers

Create precise error messages that point to the exact source location:

```elixir
defmodule MyLibrary.Verifiers.UniqueNames do
  use Spark.Dsl.Verifier

  def verify(dsl_state) do
    entities = Spark.Dsl.Extension.get_entities(dsl_state, [:my_section])

    case find_duplicate(entities) do
      nil -> :ok
      {duplicate_name, duplicate_entity} ->
        location = Spark.Dsl.Entity.anno(duplicate_entity)

        {:error,
         Spark.Error.DslError.exception(
           message: "Duplicate entity name: #{duplicate_name}",
           path: [:my_section, duplicate_name],
           module: Spark.Dsl.Verifier.get_persisted(dsl_state, :module),
           location: location
         )}
    end
  end
end
```

### Enhanced Error Messages in Transformers

```elixir
defmodule MyLibrary.Transformers.ValidateEntity do
  use Spark.Dsl.Transformer

  def transform(dsl_state) do
    entities = Spark.Dsl.Extension.get_entities(dsl_state, [:my_section])

    entities
    |> Enum.each(fn entity ->
      if invalid?(entity) do
        location = Spark.Dsl.Entity.anno(entity)

        raise Spark.Error.DslError,
          message: "Invalid configuration for #{entity.name}",
          path: [:my_section, entity.name],
          location: location
      end
    end)

    {:ok, dsl_state}
  end
end
```

### IDE Integration and Language Servers

Language servers can provide enhanced features using annotation data:

```elixir
defmodule MyLanguageServer do
  def find_definition(file, line, column) do
    # Find modules that might contain DSL at this location
    modules = find_modules_in_file(file)

    Enum.find_value(modules, fn module ->
      dsl_state = module.spark_dsl_config()

      # Check section annotations
      Enum.find_value(dsl_state, fn {path, config} ->
        if match_location?(config.section_anno, line) do
          {:section, path, config.section_anno}
        end
      end) ||

      # Check entity annotations
      find_entity_at_location(dsl_state, line)
    end)
  end
end
```

### Debugging and Development Tools

Create debugging utilities that show DSL source locations:

```elixir
defmodule MyLibrary.Debug do
  def inspect_dsl_sources(module) do
    dsl_state = module.spark_dsl_config()

    # Show all DSL elements with their locations
    Enum.each(dsl_state, fn {path, config} ->
      IO.puts("Section #{inspect(path)}:")

      if config.section_anno do
        print_location("  Section", config.section_anno)
      end

      # Show options
      Enum.each(config.opts_anno, fn {opt_name, anno} ->
        print_location("  Option #{opt_name}", anno)
      end)

      # Show entities
      Enum.each(config.entities, fn entity ->
        anno = Spark.Dsl.Entity.anno(entity)
        print_location("  Entity #{entity.name}", anno)
      end)
    end)
  end

  defp print_location(label, anno)
  defp print_location(label, nil), do: nil
  defp print_location(label, anno) do
    line = :erl_anno.location(anno)
    file = :erl_anno.file(anno) |> to_string() |> Path.relative_to_cwd()
    IO.puts("    #{label}: #{file}:#{line}")
  end
end
```

## Best Practices

### 1. Always Include Location in DslErrors

When creating DslErrors, include location information whenever available:

```elixir
# Get the appropriate annotation for your error context
location = case error_type do
  :section_error ->
    Spark.Dsl.Transformer.get_section_anno(dsl_state, path)
  :option_error ->
    Spark.Dsl.Transformer.get_opt_anno(dsl_state, path, option_name)
  :entity_error ->
    entity = Enum.at(entities, entity_index)
    Spark.Dsl.Entity.anno(entity)
end

{:error,
 Spark.Error.DslError.exception(
   message: "Clear error description",
   path: path,
   module: module,
   location: location
 )}
```

### 2. Handle Missing Annotations Gracefully

Not all annotations may be available (e.g., programmatically generated DSL
elements):

```elixir
location_info = if anno do
  line = :erl_anno.location(anno)
  file = :erl_anno.file(anno) |> to_string()
  " at #{file}:#{line}"
else
  ""
end

IO.puts("Error in entity#{location_info}")
```

### 3. Use Both Introspection and Anno Fields

- **Use introspection functions** for universal access in verifiers and
  transformers
- **Use anno fields** in entity structs for convenient access in application
  code

### 4. Check OTP Version for End Location

End location tracking requires OTP 28+:

```elixir
if function_exported?(:erl_anno, :end_location, 1) do
  end_location = :erl_anno.end_location(anno)
  # Use end location for precise span information
end
```

## Current Limitations

- Column information is not currently tracked (only line numbers)
- End Location is only tracked for OTP28+
- End Location is not available for multiline options
