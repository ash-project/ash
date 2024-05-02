# Identities

Identities are a way to declare that a record (an instance of a resource) can be uniquely identified by a set of attributes. This information can be used in various ways throughout the framework. The primary key of the resource does not need to be listed as an identity.

## Defining an identity

Identities are defined at the top level of a resource module, eg.

```elixir
defmodule MyApp.MyResource do
  use Ash.Resource #, ...
  # ...

  identities do
    # If the `email` attribute must be unique across all records
    identity :unique_email, [:email]

    # If the `username` attribute must be unique for every record with a given `site` value
    identity :special_usernames, [:username, :site]
  end
end
```

See `d:Ash.Resource.Dsl.identities` for the full range of options available when defining identities.

## Using `Ash.get`

This will allow these fields to be passed to `Ash.get/3`, e.g `Ash.get(Resource, %{email: "foo"})`.

## Using upserts

Create actions support the `upsert?: true` option, if the data layer supports it. An `upsert?` involves checking for a conflict on some set of attributes, and translating the behavior to an update in the case one is found. By default, the primary key is used when looking for duplicates, but you can set `[upsert?: true, upsert_identity: :identity_name]` to tell it to look for conflicts on a specific identity.

## Creating unique constraints

Tools like `AshPostgres` will create unique constraints in the database automatically for each identity. These unique constraints will honor other configuration on your resource, like the `base_filter` and [attribute multitenancy](/documentation/topics/multitenancy.md#attribute-multitenancy)

## Eager Checking

Setting `eager_check?: true` on an identity will allow that identity to be checked when building a create changeset over the resource. This allows for showing quick up-front validations about whether some value is taken, for example. If the resource does not have the domain configured, you can specify the domain to use with `eager_check_with: DomainName`.

If you are using `AshPhoenix.Form`, for example, this looks for a conflicting record on each call to `Form.validate/2`.
For updates, it is only checked if one of the involved fields is being changed.

For creates, The identity is checked unless your are performing an `upsert`, and the `upsert_identity` is this identity. Keep in mind that for this to work properly, you will need to pass the `upsert?: true, upsert_identity: :identity_name` _when creating the changeset_. The `primary?` read action is used to search for a record. This will error if you have not configured one.

## Pre Checking

`pre_check?` behaves the same as `eager_check?`, but it runs just prior to the action being committed. Useful for data layers that don't support transactions/unique constraints, or manual resources with identities. `Ash.DataLayer.Ets` will require you to set `pre_check?` since the ETS data layer has no built in support for unique constraints. The domain can be manually specified with `pre_check_with: DomainName`.
