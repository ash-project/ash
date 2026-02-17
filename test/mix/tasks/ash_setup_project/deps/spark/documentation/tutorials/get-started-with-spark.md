<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Spark

Spark helps you build powerful and well documented DSLs that come with useful tooling out of the box. DSLs are declared using simple structs, and every DSL has the ability to be extended by the end user. Spark powers all of the DSLs in Ash Framework.

What you get for your DSL when you implement it with Spark:

- Extensibility. Anyone can write extensions for your DSL.
- Autocomplete and in-line documentation: An elixir_sense plugin that "Just Works" for any DSL implemented with Spark.
- Tools to generate documentation for your DSL automatically.
- A mix task to add every part of the DSL to the `locals_without_parens` of your library automatically.

This library still needs a lot of documentation. It is very stable as it is used for all of the Ash packages,
but PRs are very welcome for more documentation and examples.

## Dependency

`{:spark, "~> 2.4.0"}`

## Your First DSL

Lets define a simple DSL that can be used to define data validators. We will call it `MyLibrary.Validator`.

You will be able to use it like so:

```elixir
defmodule MyApp.PersonValidator do
  use MyLibrary.Validator

  fields do
    required [:name]
    field :name, :string

    field :email, :string do
      check &String.contains?(&1, "@")
      transform &String.trim/1
    end

    # This syntax is also supported
    # field :email, :string, check: &String.contains?(&1, "@"), transform: &String.trim/1
  end
end

MyApp.PersonValidator.validate(%{name: "Zach", email: " foo@example.com "})
{:ok, %{name: "Zach", email: "foo@example.com"}}

MyApp.PersonValidator.validate(%{name: "Zach", email: " blank "})
:error
```

There are many ways you can enhance this data validator, but we are using this only as a simple realistic example.

### Defining the DSL extension

We define our DSL as a `Spark.Dsl.Extension`. All DSLs are defined in extensions, but you can define DSLs that use
certain extensions by default, which is what we will do here.

Here we are building up one big nested data structure of a `%Spark.Dsl.Section{}`, which spark
uses under the hood to allow users to write in the DSL syntax described above.

```elixir
defmodule MyLibrary.Validator.Dsl do
  defmodule Field do
    # The __spark_metadata__ field is required for Spark entities
    # It stores source location information for better error messages and tooling
    defstruct [:name, :type, :transform, :check, :__spark_metadata__]
  end

  @field %Spark.Dsl.Entity{
    name: :field,
    args: [:name, :type],
    target: Field,
    describe: "A field that is accepted by the validator",
    # you can include nested entities here, but
    # note that you provide a keyword list like below
    # we need to know which struct key to place the nested entities in
    # entities: [
    #   key: [...]
    # ],
    schema: [
      name: [
        type: :atom,
        required: true,
        doc: "The name of the field"
      ],
      type: [
        type: {:one_of, [:integer, :string]},
        required: true,
        doc: "The type of the field"
      ],
      check: [
        type: {:fun, 1},
        doc: "A function that can be used to check if the value is valid after type validation."
      ],
      transform: [
        type: {:fun, 1},
        doc: "A function that will be used to transform the value after successful validation"
      ]
    ]
  }

  @fields %Spark.Dsl.Section{
    name: :fields,
    schema: [
      required: [
        type: {:list, :atom},
        doc: "The fields that must be provided for validation to succeed"
      ]
    ],
    entities: [
      @field
    ],
    describe: "Configure the fields that are supported and required"
  }

  use Spark.Dsl.Extension, sections: [@fields]
end
```

### Defining the DSL module

Now lets define our actual DSL module. This is what people will `use` when they want to define a module that uses our DSL.

```elixir
defmodule MyLibrary.Validator do
  use Spark.Dsl,
    default_extensions: [
      extensions: [MyLibrary.Validator.Dsl]
    ]
end
```

Now, we can define something using this DSL!

```elixir
defmodule MyApp.PersonValidator do
  use MyLibrary.Validator

  fields do
    required [:name]
    field :name, :string

    field :email, :string do
      check &String.contains?(&1, "@")
      transform &String.trim/1
    end

    # This syntax is also supported
    # field :email, :string, check: &String.contains?(&1, "@"), transform: &String.trim/1
  end
end
```

### Getting information out of our DSL

For this, we can use various functions in `Spark.Dsl.Extension`. For example:

```elixir
iex(1)> Spark.Dsl.Extension.get_entities(MyApp.PersonValidator, :fields)
iex(2)> Spark.Dsl.Extension.get_entities(MyApp.PersonValidator, :fields)
[
  %MyLibrary.Validator.Dsl.Field{
    name: :name,
    type: :string,
    transform: nil,
    check: nil
  },
  %MyLibrary.Validator.Dsl.Field{
    name: :email,
    type: :string,
    transform: &String.trim/1,
    # This is an example of some under the hood magic that spark does
    # to allow you to define a function inside your DSL. This sort of thing
    # is quite difficult to do with hand-rolled DSLs.
    check: &MyApp.PersonValidator.check_0_generated_18E6D5D8C34DFA0EDA8E926DAAEE7E52/1
  }
]
```

### Getting a nice interface to your DSL

Spark provides a nice tool called `InfoGenerator` which defines functions for you automatically
corresponding to your DSL. Lets give it a whirl. The general pattern is to define a module called
`YourDsl.Info` for this.

```elixir
defmodule MyLibrary.Validator.Info do
  use Spark.InfoGenerator, extension: MyLibrary.Validator.Dsl, sections: [:fields]
end
```

Which can be used like so:

```elixir
iex(1)> MyLibrary.Validator.Info.fields(MyApp.PersonValidator)
[
  %MyLibrary.Validator.Dsl.Field{
    name: :name,
    type: :string,
    transform: nil,
    check: nil
  },
  %MyLibrary.Validator.Dsl.Field{
    name: :email,
    type: :string,
    transform: &String.trim/1,
    check: &MyApp.PersonValidator.check_0_generated_18E6D5D8C34DFA0EDA8E926DAAEE7E52/1
  }
]
# Returns `:error` for fields not specified
iex(2)> MyLibrary.Validator.Info.fields_required(MyApp.PersonValidator)
{:ok, [:name]}
# The `!` version can be used for fields you know will always be set
iex(3)> MyLibrary.Validator.Info.fields_required!(MyApp.PersonValidator)
[:name]
```

### Transformers

Transformers are an extremely powerful concept in `Spark`. They allow for arbitrary transformations
of the data structure backing our Dsl at compile time. This actually allows us to *avoid* a lot of
magic that you see in macro-based DSLs. We can write simple, regular Elixir code! For instance,
lets say we want to add an `:id` field to all validators.

```elixir
defmodule MyLibrary.Validator.Transformers.AddId do
  use Spark.Dsl.Transformer

  # dsl_state here is a map of the underlying DSL data
  def transform(dsl_state) do
    {:ok,
      Spark.Dsl.Transformer.add_entity(dsl_state, [:fields], %MyLibrary.Validator.Dsl.Field{
        name: :id,
        type: :string
      })
    }
  end
end
```

Now, we can add this transformer to our DSL extension. Modify your extension like so

```elixir
use Spark.Dsl.Extension, sections: [@fields], transformers: [
  MyLibrary.Validator.Transformers.AddId
]
```

Now our DSLs will all have an `:id` field automatically. If we recompile, we can see this in action:

```elixir
iex(1)> MyLibrary.Validator.Info.fields(MyApp.PersonValidator)
[
  %MyLibrary.Validator.Dsl.Field{
    name: :id,
    type: :string,
    transform: nil,
    check: nil
  },
  %MyLibrary.Validator.Dsl.Field{
    name: :name,
    type: :string,
    transform: nil,
    check: nil
  },
  %MyLibrary.Validator.Dsl.Field{
    name: :email,
    type: :string,
    transform: &String.trim/1,
    check: &MyApp.PersonValidator.check_0_generated_18E6D5D8C34DFA0EDA8E926DAAEE7E52/1
  }
]
```

### Verifiers

Verifiers are similar to transformers, except that they *cannot modify the structure*. They can
only return `:ok` or `{:error, error}`. This is important because when verifiers are running
you know that you are looking at the *final* structure of the DSL. Prefer to write verifiers
over transformers if you are only doing some kind of validation.

Lets make a verifier that says that all fields in `required` must also be in `fields`.

```elixir
defmodule MyLibrary.Validator.Verifiers.VerifyRequired do
  use Spark.Dsl.Verifier

  # dsl_state here is a map of the underlying DSL data
  def verify(dsl_state) do
    # we can use our info module here, even though we are passing in a
    # map of data and not a module! Very handy.

    required = MyLibrary.Validator.Info.fields_required!(dsl_state)
    fields = Enum.map(MyLibrary.Validator.Info.fields(dsl_state), &(&1.name))

    if Enum.all?(required, &Enum.member?(fields, &1)) do
      :ok
    else
      {:error,
       Spark.Error.DslError.exception(
         message: "All required fields must be specified in fields",
         path: [:fields, :required],
         # this is how you get the original module out.
         # only do this for display purposes.
         # the module is not yet compiled (we're compiling it right now!), so if you
         # try to call functions on it, you will deadlock the compiler
         # and get an error
         module: Spark.Dsl.Verifier.get_persisted(dsl_state, :module)
       )}
    end
  end
end
```

Now we can include this in our DSL extension as well!

```elixir
use Spark.Dsl.Extension,
  sections: [@fields],
  transformers: [
    MyLibrary.Validator.Transformers.AddId
  ],
  verifiers: [
    MyLibrary.Validator.Verifiers.VerifyRequired
  ]
```

Now if someone tries to define an invalid validator (yo dawg I heard you like validation),
they will get a nice error message:


```elixir
defmodule MyApp.BadValidator do
  use MyLibrary.Validator

  fields do
    required [:name, :email]
    field :name, :string
  end
end
```

produces

```
** (Spark.Error.DslError) [MyApp.BadValidator]
fields -> required:
  All required fields must be specified in fields
    (elixir 1.17.2) lib/process.ex:864: Process.info/2
    (spark 2.2.35) lib/spark/error/dsl_error.ex:30: Spark.Error.DslError.exception/1
    iex:54: MyLibrary.Validator.Verifiers.VerifyRequired.verify/1
    iex:40: anonymous fn/1 in MyApp.BadValidator.__verify_spark_dsl__/1
    (elixir 1.17.2) lib/enum.ex:4353: Enum.flat_map_list/2
    (elixir 1.17.2) lib/enum.ex:4354: Enum.flat_map_list/2
    iex:40: MyApp.BadValidator.__verify_spark_dsl__/1
    (elixir 1.17.2) lib/enum.ex:987: Enum."-each/2-lists^foreach/1-0-"/2
```

In the future we will add support for including location information in these errors,
by allowing you to look up where a given entity/option was defined in the source code,
so the user gets nice squiggly lines in their editor.

### Generating code into the module

We want each validator to have a `validate` function, so we need to generate some code.
The best way to do that is with another transformer.

```elixir
defmodule MyLibrary.Validator.Transformers.GenerateValidate do
  use Spark.Dsl.Transformer

  def transform(dsl_state) do

    validate = quote do
      def validate(data) do
        # Our generated code can be very simple
        # because we can get all the info we need from the module
        # in our regular ELixir code.
        MyLibrary.Validator.validate(__MODULE__, data)
      end
    end

    {:ok, Spark.Dsl.Transformer.eval(dsl_state, [], validate)}
  end
end
```

Now we can include this in our DSL, and then define `validate/2`,
and we're done!

```elixir
use Spark.Dsl.Extension,
  sections: [@fields],
  transformers: [
    MyLibrary.Validator.Transformers.AddId,
    MyLibrary.Validator.Transformers.GenerateValidate
  ],
  verifiers: [
    MyLibrary.Validator.Verifiers.VerifyRequired
  ]
```

Now we can define our validation implementation:

```elixir
defmodule MyLibrary.Validator do
  use Spark.Dsl,
    default_extensions: [
      extensions: [MyLibrary.Validator.Dsl]
    ]

  def validate(module, data) do
    fields = MyLibrary.Validator.Info.fields(module)
    required = MyLibrary.Validator.Info.fields_required!(module)

    case Enum.reject(required, &Map.has_key?(data, &1)) do
      [] ->
        validate_fields(fields, data)
      missing_required_fields ->
        {:error, :missing_required_fields, missing_required_fields}
    end
  end

  defp validate_fields(fields, data) do
    Enum.reduce_while(fields, {:ok, %{}}, fn field, {:ok, acc} ->
      case Map.fetch(data, field.name) do
        {:ok, value} ->
          case validate_value(field, value) do
            {:ok, value} ->
              {:cont, {:ok, Map.put(acc, field.name, value)}}
            :error ->
              {:halt, {:error, :invalid, field.name}}
          end

        :error ->
          {:cont, {:ok, acc}}
      end
    end)
  end

  defp validate_value(field, value) do
    with true <- type_check(field, value),
         true <- check(field, value) do
      {:ok, transform(field, value)}
    else
      _ ->
        :error
    end
  end

  defp type_check(%{type: :string}, value) when is_binary(value) do
    true
  end

  defp type_check(%{type: :integer}, value) when is_integer(value) do
    true
  end

  defp type_check(_, _), do: false

  defp check(%{check: check}, value) when is_function(check, 1) do
    check.(value)
  end

  defp check(_, _), do: true

  defp transform(%{transform: transform}, value) when is_function(transform, 1) do
    transform.(value)
  end

  defp transform(_, value), do: value
end
```

## Conclusion

There is *a lot* more to `spark`, but this example shows you the kinds of things that you can build,
as well as the "programming model" around `spark`. Specify a `DSL`, define instances of that `DSL`,
inspect those instances to do things like generate code or write functions like `validate/2` above!
