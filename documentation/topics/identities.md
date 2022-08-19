
    Used for indicating that some set of attributes uniquely identify a resource.

    This will allow these fields to be passed to `c:Ash.Api.get/3`, e.g `get(Resource, [some_field: 10])`,
    if all of the keys are filterable. Otherwise they are purely descriptive at the moment.
    The primary key of the resource does not need to be listed as an identity.


## eager check with

      The identity is checked on each validation of the changeset. For example, if you are using
      `AshPhoenix.Form`, this looks for a conflicting record on each call to `Form.validate/2`.
      For updates, it is only checked if one of the involved fields is being changed.

      For creates, The identity is checked unless your are performing an `upsert`, and the
      `upsert_identity` is this identity. Keep in mind that for this to work properly, you will need
      to pass the `upsert?: true, upsert_identity: :identity_name` *when creating the changeset* instead of
      passing it to the Api when creating.

      The `primary?` action is used to search for a record. This will error if you have not
      configured one.

## pre check with

      Behaves the same as `eager_check_with`, but it runs just prior to the action being committed. Useful for
      data layers that don't support transactions/unique constraints, or manual resources with identities.