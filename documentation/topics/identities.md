# Identities

Identities are a way to declare that a record (an instance of a resource) can be uniquely identified by a set of attributes. This information can be used in various ways throughout the framework. The primary key of the resource does not need to be listed as an identity.

## Using Api.get

This will allow these fields to be passed to `c:Ash.Api.get/3`, e.g `get(Resource, [email: "foo"])`.

## Using upserts

Create actions support the `upsert?: true` option, if the data layer supports it. An `upsert?` involves checking for a conflict on some set of attributes, and translating the behavior to an update in the case one is found. By default, the primary key is used when looking for duplicates, but you can set `[upsert?: true, upsert_identity: :identity_name]` to tell it to look for conflicts on a specific identity.

## Creating unique constraints

Tools like `AshPostgres` will create unique constraints in the database automatically for each identity. These unique constraints will honor other configuration on your resource, like the `base_filter`.

## Eager Checking

Setting `eager_check_with: ApiName` on an identity will allow that identity to be checked when building a create changeset over the resource. This allows for showing quick up-front validations about wether some value is taken, for example.

If you are using `AshPhoenix.Form`, for example, this looks for a conflicting record on each call to `Form.validate/2`.
For updates, it is only checked if one of the involved fields is being changed.

For creates, The identity is checked unless your are performing an `upsert`, and the `upsert_identity` is this identity. Keep in mind that for this to work properly, you will need to pass the `upsert?: true, upsert_identity: :identity_name` *when creating the changeset* instead of passing it to the Api when creating. The `primary?` read action is used to search for a record. This will error if you have not configured one.

## Pre Checking

`pre_check_with: ApiName` behaves the same as `eager_check_with`, but it runs just prior to the action being committed. Useful for data layers that don't support transactions/unique constraints, or manual resources with identities. `Ash.DataLayer.Ets` will actually require you to set `pre_check_with` since the ETS data layer has no built in support for unique constraints.
