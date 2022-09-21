# Extending Resource

Resource extensions allow you to make powerful modifications to resources, and extend the DSL to configure how those modifications are made. If you are using `AshPostgres`, `AshGraphql` or `AshJsonApi`, they are all integrated into a resource using extensions. In this guide we will build a simple extension that adds timestamps to your resource. We'll also show some simple patterns that can help ensure that all of your resources are using your extension.

## Creating an extension

Extensions are modules that expose a set of DSL Transformers and DSL Sections. We'll start with the transformers.

Here we create an extension called `MyApp.Extensions.Base`, and configure a single transformer, called `MyApp.Extensions.Base.AddTimestamps`

```elixir
defmodule MyApp.Extensions.Base do
  use Spark.Dsl.Extension, transformers: [MyApp.Extensions.Base.AddTimestamps]
end
```

## Creating a transformer

Transformers are all run serially against a map of data called `dsl_state`,  which is the data structure that we build as we use the DSL. For example:

```elixir
attributes do
  attribute :name, :string
end
```

Would, under the hood might look like this:

```elixir
%{
  [:attributes] => %{entities: [
      %Ash.Resource.Attribute{name: :name, type: :string}
    ]
  },
  ...
}
```

`Spark.Dsl.Transformer` provides utilities to work with this data structure, and most introspection utilities also work with with that data structure (i.e `Ash.Resource.Info.attributes(dsl_state)`). A transformer exposes `transform/1`, which takes the `dsl_state` and returns either `{:ok, dsl_state}` or `{:error, error}`

```elixir
defmodule MyApp.Extensions.Base.AddTimestamps do
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    {:ok, inserted_at} =
      Transformer.build_entity(Ash.Resource.Dsl, [:attributes], :create_timestamp,
        name: :inserted_at
      )

    {:ok, updated_at} =
      Transformer.build_entity(Ash.Resource.Dsl, [:attributes], :update_timestamp,
        name: :updated_at
      )

    {:ok,
     dsl_state
     |> Transformer.add_entity([:attributes], inserted_at)
     |> Transformer.add_entity([:attributes], updated_at)}
  end
end

```

This transformer builds and adds a `create_timestamp` called `:inserted_at` and an `update_timestamp` called `:updated_at`.

### Introspecting the resource

If the resource we are extending already has an attribute called `inserted_at` or `updated_at`, we'd most likely want to avoid adding one ourselves (this would cause a compile error about duplicate attribute names). We can check for an existing attribute and make that change like so:

```elixir
  def transform(dsl_state) do
    {:ok,
      dsl_state
      |> add_attribute_if_not_exists(:create_timestamp, :inserted_at)
      |> add_attribute_if_not_exists(:update_timestamp, :updated_at)}
  end

  defp add_attribute_if_not_exists(dsl_state, type, name) do
    if Ash.Resource.Info.attribute(dsl_state, name) do
      dsl_state
    else
      {:ok, attribute} =
        Transformer.build_entity(Ash.Resource.Dsl, [:attributes], type,
          name: name
        )

      dsl_state
      |> Transformer.add_entity([:attributes], attribute)
    end
  end
```

This is just one example of what you can do with transformers. Check out the functions in `Spark.Dsl.Transformer` to see what utilities are available.

### Make the extension configurable

So far we've covered transformers, and using them to modify resources, but now lets say we want to make this behavior opt-out. Perhaps certain resources really shouldn't have timestamps, but we want it to be the default. Lets add a "DSL Section" to our extension.

```elixir
defmodule MyApp.Extensions.Base do
  @base %Ash.Dsl.Section{
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

  defmodule Info do
    def timestamps?(resource) do
      Spark.Dsl.Extension.get_option(resource, [:base], :timestamps?, true)
    end
  end

  use Spark.Dsl.Extension, 
    transformers: [MyApp.Extensions.Base.AddTimestamps],
    sections: [@base]
end
```

Now we can use this configuration in our transformer, like so:

```elixir
  def transform(dsl_state) do
    if MyApp.Extensions.Base.Info.timestamps?(dsl_state) do
      {:ok,
        dsl_state
        |> add_attribute_if_not_exists(:create_timestamp, :inserted_at)
        |> add_attribute_if_not_exists(:update_timestamp, :updated_at)}
    else
      {:ok, dsl_state}
    end
  end

  defp add_attribute_if_not_exists(dsl_state, type, name) do
    if Ash.Resource.Info.attribute(dsl_state, name) do
      dsl_state
    else
      {:ok, attribute} =
        Transformer.build_entity(Ash.Resource.Dsl, [:attributes], type,
          name: name
        )

      dsl_state
      |> Transformer.add_entity([:attributes], attribute)
    end
  end
```

And now we have a configurable base extension

### A note on the ordering of transformers

In this case, this transformer can run in any order. However, as we start adding transformers and/or modify the behavior of this one, we may need to ensure that our transformer runs before or after specific transformers. As of the writing of this guide, the best way to look at the list of transformers is to look at the source of the extension, and see what transformers it has and what they do. The [Resource DSL](https://github.com/ash-project/ash/blob/main/lib/ash/resource/dsl.ex) for example.

If you need to affect the ordering, you can define `before?/1` and `after?/1` in your transformer, i.e

```elixir
# I go after any other transformer
def after?(_), do: true

# except I go before `SomeOtherTransformer`
def before?(SomeOtherTransformer), do: true
def before?(_), do: false
```

## Using your extension

Now it can be used like any other extension:

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

Your extension will be automatically supported by the `elixir_sense` extension, showing inline documentation and auto complete as you type. For more on that, see {{link:ash:guide:Development Utilities}}.

## Making a Base Resource

The "Base Resource" pattern has been adopted by some as a way to make it easy to ensure that your base extension is used everywhere. Instead of using `Ash.Resource` you use `MyApp.Resource`. Take a look at the {{link:ash:guide:Development Utilities}} guide if you do this, as you will need to update your formatter configuration, if you are using it.

```elixir
defmodule MyApp.Resource do
  defmacro __using__(opts) do
    quote do
      use Ash.Resource,
        unquote(Keyword.update(opts, :extensions, [MyApp.Extensions.Base], &[MyApp.Extensions.Base | &1]))
    end
  end
end
```

And now you can use it with your resources like this:

```elixir
defmodule MyApp.Tweet do
  use MyApp.Resource
end
```

## Ensuring that all resources use your base extension

To do this, you could create an extension very similar to `Ash.Registry.ResourceValidations`, that ensures that any resource present uses your extension. `Spark.extensions/1` can be used to see what extensions a given module or `dsl_config` has adopted.
