<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Multitenancy

Multitenancy is the splitting up your data into discrete areas, typically by
customer. One of the most common examples of this, is the idea of splitting up a
postgres database into "schemas" one for each customer that you have. Then, when
making any queries, you ensure to always specify the "schema" you are querying,
and you never need to worry about data crossing over between customers. The
biggest benefits of this kind of strategy are the simplification of
authorization logic, and better performance. Instead of all queries from all
customers needing to use the same large table, they are each instead all using
their own smaller tables. Another benefit is that it is much easier to delete a
single customer's data on request.

In Ash, there are two strategies for implementing multitenancy. The first (and
simplest) strategy works for any data layer that supports filtering, and
requires little maintenance/mental overhead. It is done via expecting a given
attribute to line up with the `tenant`, and is called the `:attribute` strategy.
The second, is based on the data layer backing your resource, and is called the
`:context` strategy. For information on context based multitenancy, see the
documentation of your data layer. For example, `AshPostgres` uses postgres
schemas. While the `:attribute` strategy is simple to implement, it offers fewer
advantages, primarily acting as another way to ensure your data is filtered to
the correct tenant.

## Setting Tenant

### Using Ash.PlugHelpers.set_tenant

You can use `Ash.PlugHelpers.set_tenant/2` in your plug pipeline to set the
tenant for all operations:

Example usage of the above:

```elixir
conn
|> Ash.PlugHelpers.set_tenant(tenant)
```

**Important:** If you're using `ash_authentication` with Multitenant User or
Token resources, the `Ash.PlugHelpers.set_tenant` plug **must be placed before
any authentication plugs** in your pipeline. This ensures the tenant is
available when authentication operations need to query or create user/token
records.

### Using Ash.Scope

You can also use `Ash.Scope` to you can group up actor/tenant/context into one
struct and pass that around.

Example usage of the above:

```elixir
scope = %MyApp.Scope{current_user: user, current_tenant: tenant, locale: "en"}
MyApp.Blog.create_post!("new post", scope: scope)

```

See the Guide [guide](https://hexdocs.pm/ash/Ash.Scope.html).

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

In this case, if you were to try to run a query without specifying a tenant, you
would get an error telling you that the tenant is required.

Setting the tenant is done via `Ash.Query.set_tenant/2` and
`Ash.Changeset.set_tenant/2`. If you are using a
[code interface](/documentation/topics/resources/code-interfaces.md), you can
pass `tenant:` in the options list (the final parameter). If you are using an
extension, such as `AshJsonApi` or `AshGraphql` the method of setting tenant
context is explained in that extension's documentation.

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

If you want to enable running queries _without_ a tenant as well as queries with
a tenant, the `global?` option supports this. You will likely need to
incorporate this ability into any authorization rules though, to ensure that
users from one tenant can't access other tenant's data.

```elixir
multitenancy do
  strategy :attribute
  attribute :organization_id
  global? true
end
```

### Transforming Tenant Values

You can provide the `parse_attribute` option if the tenant being set doesn't
exactly match the attribute value. For example, if the tenant is `"org_10"` and
the attribute is `organization_id`, which requires just `10`.

```elixir
defmodule MyApp.Users do
  use Ash.Resource, ...

  multitenancy do
    strategy :attribute
    attribute :organization_id
    parse_attribute {MyApp.Users, :parse_tenant, []}
  end

  def parse_tenant("org_" <> id), do: String.to_integer(id)
  def parse_tenant(id) when is_integer(id), do: id
end
```

The inverse transformation can be configured with `tenant_from_attribute`, which
takes an attribute value and returns the tenant. This is primarily used by
extensions that need to convert an attribute value back to its tenant
representation.

```elixir
defmodule MyApp.Users do
  use Ash.Resource, ...

  multitenancy do
    strategy :attribute
    attribute :organization_id
    parse_attribute {MyApp.Users, :parse_tenant, []}
    tenant_from_attribute {MyApp.Users, :format_tenant, []}
  end

  # Transforms tenant -> attribute value
  def parse_tenant("org_" <> id), do: String.to_integer(id)
  def parse_tenant(id) when is_integer(id), do: id

  # Transforms attribute value -> tenant (inverse of parse_tenant)
  def format_tenant(id) when is_integer(id), do: "org_#{id}"
end
```

**Note:** The `parse_attribute` and `tenant_from_attribute` functions should be
inverses of each other for consistent behavior across extensions.

## Tenant-Aware Identities

When using identities in a multitenant resource, the tenant attribute is
automatically included in the uniqueness constraints. This means that the same
identity value can exist across different tenants, but must be unique within a
single tenant. For example, if you have a `User` resource with an email
identity, users in different organizations could have the same email address.

If you need an identity to be globally unique across all tenants (like a global
user email system), you can set `all_tenants?: true` on the identity.

```elixir
defmodule MyApp.User do
  use Ash.Resource, ...

  multitenancy do
    strategy :attribute
    attribute :organization_id
  end

  identities do
    # This email must be unique within a tenant
    identity :tenant_scoped_email, [:email]

    # This username must be unique across all tenants
    identity :global_username, [:username], all_tenants?: true
  end
end
```

Example implications:

```elixir
# These are valid because they're in different tenants
User
|> Ash.Changeset.for_create(:create, %{email: "fred@example.com"})
|> Ash.Changeset.set_tenant(1)
|> Ash.create!()

User
|> Ash.Changeset.for_create(:create, %{email: "fred@example.com"})
|> Ash.Changeset.set_tenant(2)
|> Ash.create!()

# This would fail because usernames are global
User
|> Ash.Changeset.for_create(:create, %{username: "fred"})
|> Ash.Changeset.set_tenant(1)
|> Ash.create!()

User
|> Ash.Changeset.for_create(:create, %{username: "fred"})
|> Ash.Changeset.set_tenant(2)
|> Ash.create!() # Error: username already taken
```

## Context Multitenancy

Context multitenancy allows for the data layer to dictate how multitenancy
works. For example, a csv data layer might implement multitenancy via saving the
file with different suffixes, or an API wrapping data layer might use different
subdomains for the tenant.

For `AshPostgres` context multitenancy, which uses postgres schemas and is
referred to ash "Schema Based Multitenancy", see the
[guide](https://hexdocs.pm/ash_postgres/schema-based-multitenancy.html)

## Possible Values for tenant

By default, the tenant value is passed directly to the relevant implementation.
For example, if you are using schema multitenancy with `ash_postgres`, you might
provide a schema like `organization.subdomain`. In Ash, a tenant should be
identifiable by a single value, like an integer or a string.

You can use the `Ash.ToTenant` protocol to automatically convert values into
this simple value. The example below will allow you to use the same organization
everywhere, and have it automatically converted into the correct schema for
postgres, and the correct id for attribute-based multitenant resources. You can
use this without looking up the relevant record as well, as long as the required
fields used in your protocol are present.

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

This allows you to pass an `%Organization{}` or an organization_id around, and
have that `organization_id` properly used with attribute and context-based
multitenancy.
