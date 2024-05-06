# Writing Extensions

Extensions allow you to make powerful modifications to DSL entities. If you are using `AshPostgres`, `AshGraphql` or `AshJsonApi`, they are all integrated into Ash using extensions. In this guide we will build a simple extension for `Ash.Resource` that adds timestamps to your resource. We'll also show some simple patterns that can help ensure that all of your resources are using your extension.

## Creating an extension

What we call an "extension" is typically one or more `Spark.Dsl.Extension`, and then any additional code that is used by that extension. For example, `AshGraphql` has a domain extension called `AshGraphql.Domain`, a resource extension called `AshGraphql.Resource`, and code to connect a GraphQL schema to your resources.

### DSL Extension

Here we create a DSL extension called `MyApp.Extensions.Base`, and configure a single transformer, called `MyApp.Extensions.Base.AddTimestamps`

```elixir
defmodule MyApp.Extensions.Base do
  use Spark.Dsl.Extension, transformers: [MyApp.Extensions.Base.AddTimestamps]
end
```

### Transformers

Transformers are all run serially against a map of data called `dsl_state`, which is the data structure that we build as we use the DSL. For example:

```elixir
attributes do
  attribute :name, :string
end
```

Would, under the hood, look something like this:

```elixir
%{
  [:attributes] => %{entities: [
      %Ash.Resource.Attribute{name: :name, type: :string}
    ]
  },
  ...
}
```

A transformer exposes `transform/1`, which takes the `dsl_state` and returns either `{:ok, dsl_state}` or `{:error, error}`

```elixir
defmodule MyApp.Extensions.Base.AddTimestamps do
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    dsl_state
    # Ash.Resource.Builder has utilities for extending resources
    |> Ash.Resource.Builder.add_new_create_timestamp(:inserted_at)
    |> Ash.Resource.Builder.add_new_update_timestamp(:updated_at)
  end
end
```

This transformer builds adds a `create_timestamp` called `:inserted_at` and an `update_timestamp` called `:updated_at`, unless they already exist.

### Make the extension configurable

So far we've covered transformers, and using them to modify resources, but now lets say we want to make this behavior opt-out. Perhaps certain resources really shouldn't have timestamps, but we want it to be the default. Lets add a "DSL Section" to our extension.

```elixir
defmodule MyApp.Extensions.Base do
  @base %Spark.Dsl.Section{
    name: :base,
    describe: """
    Configure the behavior of our base extension.
    """,
    examples: [
      """
      base do
        timestamps? false
      end
      """
    ],
    schema: [
      timestamps?: [
        type: :boolean,
        doc: "Set to false to skip adding timestamps",
        default: true
      ]
    ]
  }



  use Spark.Dsl.Extension,
    transformers: [MyApp.Extensions.Base.AddTimestamps],
    sections: [@base]
end

defmodule MyApp.Extensions.Base.Info do
  use Spark.InfoGenerator, extension: MyApp.Extensions.Base, sections: [:base]

  # This will define `base_timestamps?/1`.
end
```

Now we can use this configuration in our transformer, like so:

```elixir
  def transform(dsl_state) do
    # Introspection functions can take a `dsl_state` *or* a module
    if MyApp.Extensions.Base.Info.base_timestamps?(dsl_state) do
      dsl_state
      |> Ash.Resource.Builder.add_new_create_timestamp(:inserted_at)
      |> Ash.Resource.Builder.add_new_update_timestamp(:updated_at)
    else
      {:ok, dsl_state}
    end
  end
```

And now we have a configurable base extension! For more information on writing DSLs, see `Spark`.  `Spark` is still lacking in documentation, unfortunately, as its something that mostly the adventurous/power users work with, and they often learn by way of examples, looking at `Ash` extensions. We would like to rectify this in the future. Please reach out if you're interested in assisting with that effort!

### Ordering of transformers

In this case, this transformer can run in any order. However, as we start adding transformers and/or modify the behavior of this one, we may need to ensure that our transformer runs before or after specific transformers. As of the writing of this guide, the best way to look at the list of transformers is to look at the source of the extension, and see what transformers it has and what they do. The [Resource DSL](https://github.com/ash-project/ash/blob/main/lib/ash/resource/dsl.ex) for example.

If you need to affect the ordering, you can define `before?/1` and `after?/1` in your transformer, i.e

```elixir
# I go after every transformer
def after?(_), do: true

# except I go before `SomeOtherTransformer`
def before?(SomeOtherTransformer), do: true
def before?(_), do: false
```

## Using your extension

Now it can be used in a resource:

```elixir
defmodule MyApp.Tweet do
  use Ash.Resource,
    extensions: [MyApp.Extensions.Base]

  base do
    # And you can configure it like so
    timestamps? false
  end
end
```

Your extension will automatically support autocompletion if using `ElixirLS`, showing inline documentation and type-aware auto complete as you type. For more, see [Development Utilities](/documentation/topics/development/development-utilities.md)
