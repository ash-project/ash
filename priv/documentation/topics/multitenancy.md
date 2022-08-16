
# strategy
        Determine how to perform multitenancy. `:attribute` will expect that an
        attribute matches the given `tenant`, e.g `org_id`. `context` (the default)
        implies that the tenant will be passed to the datalayer as context. How a
        given data layer handles multitenancy will differ depending on the implementation.
        See the datalayer documentation for more.


# global?

This allows running queries
        and making changes without setting a tenant. This may eventually be extended to support
        describing the relationship to global data. For example, perhaps the global data is
        shared among all tenants (requiring "union" support in data layers), or perhaps global
        data is "merged" using some strategy (also requiring "union" support).