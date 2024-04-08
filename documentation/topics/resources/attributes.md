# Attributes

Attributes specify the `name`, `type` and additional configuration of a simple property of a record. When using SQL data layers, for example, an attribute would correspond to a column in a database table. For information on types, see `Ash.Type`.

To see all of the options available when building attributes, see `d:Ash.Resource.Dsl.attributes.attribute`

If you are looking to compute values on demand, see the [Calculations guide](/documentation/topics/resources/calculations.md) and the [aggregates guide](/documentation/topics/resources/aggregates.md).

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
  writable? false
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
  writable? false
  default &DateTime.utc_now/0
  update_default &DateTime.utc_now/0
  match_other_defaults? true
  allow_nil? false
end
```

### `uuid_primary_key`

This attribute is used in almost every resource. It generates a UUID every time a new record is made.
`uuid_primary_key :id` is equivalent to:

```elixir
attribute :id, :uuid do
  writable? false
  default &Ash.UUID.generate/0
  primary_key? true
  allow_nil? false
end
```

### `integer_primary_key`

Creates a generated integer primary key. Keep in mind that not all data layers support auto incrementing ids, but for SQL data layers this is a very common practice. For those that don't, it is your own job to provide values for the primary key. We generally suggest using UUIDs over integers, as there are [a lot of good reasons to not use autoincrementing integer ids](https://www.clever-cloud.com/blog/engineering/2015/05/20/why-auto-increment-is-a-terrible-idea/).

`integer_primary_key :id` is equivalent to:

```elixir
attribute :id, :integer do
  writable? false
  generated? true
  primary_key? true
  allow_nil? false
end
```
