# Embedded Resources

Embedded resources function very similarly to [embedded schemas in Ecto](https://hexdocs.pm/ecto/Ecto.Schema.html).
The primary difference is the same as the primary difference between Ecto schemas and Ash resources: the full lifecycle
of the resource is managed by its configuration. For example, you can add validations, calculations, and even authorization policies to an embedded resource. Here is an example of a simple embedded resource:

```elixir
defmodule MyApp.Profile do
  use Ash.Resource,
    data_layer: :embedded # Use the atom `:embedded` as the data layer.

  attributes do
    attribute :first_name, :string
    attribute :last_name, :string
  end
end
```

Embedded resources cannot have relationships or aggregates.

## Adding embedded resource attributes

Embedded resources simply define an `Ash.Type` under the hood, meaning you can use them anywhere you would 
use an Ash type.

```elixir
defmodule MyApp.User do
  use Ash.Resource, ...

  attributes do
    ...

    attribute :profile, MyApp.Profile
    attribute :profiles, {:array, MyApp.Profile} # You can also have an array of embeds
  end
end
```

## Editing embedded attributes

If you manually supply instances of the embedded structs, the structs you provide are used with no validation. For example:

```elixir
Ash.Changeset.new(user, %{profile: %MyApp.Profile{first_name: "first_name", last_name: "last_name}})
```

However, you can also treat embedded resources like regular resources that can be "created", "updated", and "destroyed".
To do this, provide maps as the input, instead of structs. In the example above, if you just wanted to change the `first_name`, you'd provide:

```elixir
Ash.Changeset.new(user, %{profile: %{first_name: "first_name"}})
```

This will call the primary `update` action on the resource. This allows you to define an action on the embed, and add validations to it. For example, you might have something like this:

```elixir
defmodule MyApp.Profile do
  use Ash.Resource,
    data_layer: :embedded # Use the atom `:embedded` as the data layer.

  attributes do
    attribute :first_name, :string
    attribute :last_name, :string
  end

  validations do
    validate present([:first_name, :last_name], at_least: 1)
  end
end
```

## Calculations

Calculations can be added to embedded resources. When you use an embedded resource, you declare what calculations to load via a `constraint`.
For example:

```elixir
defmodule MyApp.Profile do
  use Ash.Resource,
    data_layer: :embedded # Use the atom `:embedded` as the data layer.

  attributes do
    attribute :first_name, :string
    attribute :last_name, :string
  end

  calculations do
    calculate :full_name, :string, concat([:first_name, :last_name], " ")
  end
end

defmodule MyApp.User do
  use Ash.Resource,
    ...

  attributes do
    attribute :profile, MyApp.Profile do
      constraints [load: [:full_name]]
    end
  end
end
```

## Determining what action(s) will be called:

Remember: default actions are already implemented for a resource, with no need to add them. They are called `:create`, `:update`, `:destroy`, and `:read`. You can use those without defining them. You only need to define them if you wish to override their configuration.

### Single Embeds

* If the current value is `nil` - a `create` with the provided values
* If the current value is not `nil` - an `update` with the provided values
* If the current value is not `nil` and the *new value* is `nil` - a `destroy` with the original value

### Array Embeds

All values in the original array are destroyed, and all maps in the new array are used to `create` new records.

## Adding a primary key

Adding a primary key to your embedded resources is especially useful when managing lists of data. Specifically, it allows you to consider changes to elements with matching primary key values as `updates`.

For example:

```elixir
defmodule MyApp.Tag do
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    uuid_primary_key :id
    attribute :name, :string
    attribute :counter, :integer
  end

  validations do
    validate {Increasing, field: :counter}, on: :update
  end
end
```

Now, you can accept input meant to `update` individual list items. The entire list must still be provided, but any items with a matching id will be considered an `update` instead of a `destroy` + `create`.

### Determining what action(s) will be called with a primary key:

#### Single Embeds with primary keys

* If you provide a struct, instead of a map, the value provided is simply used as the new relationship value.
* If the current value is `nil` - a `create` with the provided values
* If the current value is not `nil` and the primary keys match - an `update` with the provided values
* If the current value is not `nil` and the primary keys *don't* match - a `destroy` of the original value and a `create` of the new value
* If the current value is not `nil` and the *new value* is `nil` - a `destroy` with the original value

#### Array Embeds with primary keys

* If you provide structs, instead of maps, the value provided is simply used as the new relationship value.
* Any values in the original list with no primary key matching in the new list are `destroy`ed.
* Any values in the new list with no primary key matching in the original list are `create`d.
* Any values with a primary key match in the original list and the new list are `update`d

## Identities

Identities can be added on an embedded resource, which will ensure that for any given list, they are unique on that identity. For example, if you had an embedded resource called `Tag`, you could add an identity on `name` to ensure that nothing has duplicate tag names.

## Usage in Extensions

The AshJsonApi extension simply exposes these attributes as maps. However, the AshGraphql extension allows you
to specify a type (but not queries/mutations) for an embedded resource. If you do, instead of being treated as a `:json` type it will get its own named input object type and field type.

## Accessing the source changeset

When building changesets for embedded resources, the source changeset will be available in action changes under `changeset.context.__source__`.
This allows you to customize the action based on the details of the parent changeset.