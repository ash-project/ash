# Multitenancy

Multitenancy is the splitting up your data into discrete areas, typically by customer. One of the most common examples of this, is the idea of splitting up a postgres database into "schemas" one for each customer that you have. Then, when making any queries, you ensure to always specify the "schema" you are querying, and you never need to worry about data crossing over between customers. The biggest benefits of this kind of strategy are the simplification of authorization logic, and better performance. Instead of all queries from all customers needing to use the same large table, they are each instead all using their own smaller tables. Another benefit is that it is much easier to delete a single customer's data on request.

In Ash, there are two primary strategies for implementing multitenancy. The first (and simplest) works for any data layer that supports filtering, and requires very little maintenance/mental overhead. It is done via expecting a given attribute to line up with the `tenant`, and is called `:attribute`. The second, is based on the data layer backing your resource, and is called `:context`. For information on context based multitenancy, see the documentation of your data layer. For example, `AshPostgres` uses postgres schemas. While the `:attribute` strategy is simple to implement, it also offers fewer advantages, primarily acting as another way to ensure your data is filtered to the correct tenant.

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

Setting the tenant is done via `Ash.Query.set_tenant/2` and `Ash.Changeset.set_tenant/2`. If you are using a [code interface](/documentation/topics/resources/code-interfaces.md), you can pass `tenant: ` in the options list (the final parameter). If you are using an extension, such as `AshJsonMyDomain` or `AshGraphql` the method of setting tenant context is explained in that extension's documentation.

Example usage of the above:

```elixir
# Error when not setting a tenant
MyApp.Users
|> Ash.Query.filter(name == "fred")
|> Ash.read!()
** (Ash.Error.Invalid)

* "Queries against the Helpdesk.Accounts.User resource require a tenant to be specified"
    (ash 1.22.0) lib/ash/domain/domain.ex:944: Ash.Domain.unwrap_or_raise!/2

# Automatically filtering by `organization_id == 1`
MyApp.Users
|> Ash.Query.filter(name == "fred")
|> Ash.Query.set_tenant(1)
|> Ash.read!()

[...]

# Automatically setting `organization_id` to `1`
MyApp.Users
|> Ash.Changeset.for_create(:create, %{name: "fred"})
|> Ash.Changeset.set_tenant(1)
|> Ash.create!()

%MyApp.User{organization_id: 1}

# Setting tenant with a code interface that exposes `list_all` as a read action
MyApp.Users.list_all(tenant: 1)
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

Context multitenancy allows for the data layer to dictate how multitenancy works. For example, a csv data layer might implement multitenancy via saving the file with different suffixes, or an API wrapping data layer might use different subdomains for the tenant.

For `AshPostgres` context multitenancy, which uses postgres schemas and is referred to ash "Schema Based Multitenancy", see the [guide](https://hexdocs.pm/ash_postgres/schema-based-multitenancy.html)

## Possible Values for tenant

By default, the tenant value is passed directly to the relevant implementation. For example, if you are using schema multitenancy with `ash_postgres`, you might provide a schema like `organization.subdomain`. In Ash, a tenant should be identifiable by a single value, like an integer or a string.

You can use the `Ash.ToTenant` protocol to automatically convert values into this simple value. The example below will allow you to use the same organization everywhere, and have it automatically converted into the correct schema for postgres, and the correct id for attribute-based multitenant resources. You can use this without looking up the relevant record as well, as long as the required fields used in your protocol are present.

```elixir
Ash.Changeset.for_create(..., tenant: %MyApp.Organization{id: id})
```

```elixir
# in Organization resource

defimpl Ash.ToTenant do
  def to_tenant(%{id: id}, resource) do
    if Ash.Resource.Info.data_layer(resource) == AshPostgres.DataLayer
      && Ash.Resource.Info.multitenancy_strategy(resource) == :context do
      "org_#{id}"
    else
      id
    end
  end
end
```

This allows you to pass an `%Organization{}` or an organization_id around, and have that `organization_id` properly used with attribute and context-based multitenancy.
