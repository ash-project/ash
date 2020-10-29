# Multitenancy

Multitenancy is the idea of splitting up your data into discrete areas, typically by customer. One of the most common examples of this, is the idea of splitting up a postgres database into "schemas" one for each customer that you have. Then, when making any queries, you ensure to always specify the "schema" you are querying, and you never need to worry about data crossing over between customers. The biggest benefits of this kind of strategy are the simplification of authorization logic, and better performance. Instead of all queries from all customers needing to use the same large table, they are each instead all using their own smaller tables. Another benefit is that it is much easier to delete a single customer's data on request.

In Ash, there are a two primary strategies for implementing multitenancy. The first (and simplest) works for any data layer that supports filtering, and requires very little maintenance/mental overhead. It is done via expecting a given attribute to line up with the `tenant`, and is called `:attribute`. The second, is based on the data layer backing your resource, and is called `:context`. For information on
context based multitenancy, see the documentation of your datalayer. For example, `AshPostgres` uses postgres schemas. While the `:attribute` strategy is simple to implement, it also offers fewer advantages, primarily acting as another way to ensure your data is filtered to the correct tenant.

## Attribute Multitenancy

```elixir
defmodule MyApp.Users do
  use Ash.Resource, ...

  multitenancy do
    strategy :attribute
    attribute :organization_id
  end

  ...

  relationships do
    belongs_to :organization, MyApp.Organization
  end
end
```

In this case, if you were to try to run a query without specifying a tenant, you would get an error telling you that the tenant is required.

Setting the tenant when using the code API is done via `Ash.Query.set_tenant/2` and `Ash.Changeset.set_tenant/2`. If you are using an extension, such as `AshJsonApi` or `AshGraphql` the method of setting tenant context is explained in that extension's documentation.

Example usage of the above:

```elixir
# Error when not setting a tenant
MyApp.Users
|> Ash.Query.filter(name == "fred")
|> MyApi.read!()
** (Ash.Error.Unknown)

* "Queries against the Helpdesk.Accounts.User resource require a tenant to be specified"
    (ash 1.22.0) lib/ash/api/api.ex:944: Ash.Api.unwrap_or_raise!/2

# Automatically filtering by `organization_id == 1`
MyApp.Users
|> Ash.Query.filter(name == "fred")
|> Ash.Query.set_tenant(1)
|> MyApi.read!()

[...]

# Automatically setting `organization_id` to `1`
MyApp.Users
|> Ash.Changeset.new(name: "fred")
|> Ash.Changeset.set_tenant(1)
|> MyApi.create!()

%MyApp.User{organization_id: 1}
```

If you want to enable running queries _without_ a tenant as well as queries with a tenant, the `global?` option supports this. You will likely need to incorporate this ability into any authorization rules though, to ensure that users from one tenant can't access other tenant's data.

```elixir
multitenancy do
  strategy :attribute
  attribute :organization_id
  global? true
end
```

You can also provide the `parse_attribute?` option if the tenant being set doesn't exactly match the attribute value, e.g the tenant is `org_10` and the attribute is `organization_id`, which requires just `10`.

## Context Multitenancy

For `AshPostgres` multitenancy, see the [guide](https://hexdocs.pm/ash_postgres/multitenancy.html)
