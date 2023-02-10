# Attributes

Attributes specify the name, type and options of a piece of information in a resource.

## Ways of writing attributes

There are two ways to write an attribute:

```elixir
defmodule MyProject.MyApi.MyResource do
  use Ash.Resource, data_layer: ...

  actions do

  end

  attributes do
    attribute :name, :string, allow_nil?: false

    # or ...
    attribute :name, :string do
      allow_nil? false
    end
  end
end
```

Both ways will work. Though when you're using many options the latter is preferred.

For more information on attribute types including composite types and defining your own custom type see t:Ash.Type

You can find a comprehensive of attribute options with detailed descriptions on the d:attribute page.

## Special attributes

In Ash there are 4 special attributes these are:

- `create_timestamp`
- `update_timestamp`
- `integer_primary_key`
- `uuid_primary_key`

These are really just shorthand for an attribute with specific options set. They're outlined below.

### `create_timestamp`

You may recognise this if you have used Ecto before. This attribute will record the time at which each row is created, by default it uses `DateTime.utc_now/1`.

`create_timestamp :inserted_at` is equivalent to an attribute with these options:

```elixir
attribute :inserted_at, :utc_datetime_usec do
  writeable? false
  private? true
  default &DateTime.utc_now/0
  match_other_defaults? true
  allow_nil? false
end
```

### `update_timestamp`

This is also similar in Ecto. This attribute records the last time a row was updated, also using `DateTime.utc_now/1` by default.

`update_timestamp :updated_at` is equivalent to:

```elixir
attribute :updated_at, :utc_datetime_usec do
  writeable? false
  private? true
  default &DateTime.utc_now/0
  updated_default &DateTime.utc_now/0
  match_other_defaults? true
  allow_nil? false
end
```

### `uuid_primary_key`

This attribute is used in almost every resource. It generates a UUID every time a new record is made.
`uuid_primary_key :id` is equivalent to:

```elixir
attribute :id, :uuid do
  writeable? false
  default &Ash.UUID.generate/0
  primary_key? true
  allow_nil? false
end
```

### `integer_primary_key`

Don't use this attribute unless absolutely necessary, there are [a lot of good reasons to not use autoincrementing integer ids](https://www.clever-cloud.com/blog/engineering/2015/05/20/why-auto-increment-is-a-terrible-idea/). If you do, _please_ make sure these resource are only accessed internally and aren't exposed via a public API.

`integer_primary_key :id` is equivalent to:

```elixir
attribute :id, :integer do
  writeable? false
  generated? true
  primary_key? true
  allow_nil? false
end
```
