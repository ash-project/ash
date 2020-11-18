# Identities

Identities can be used to describe the ways that a resource is uniquely identified. For example, you may have a user resource that has an `id` primary key, but is uniquely identifiable via the `email` attribute as well.
To configure this, add an `identities` block inside of the `resource` block.

For example:

```elixir
resource do
  identities do
    identity :unique_email, [:email]
  end
end
```

## Effects

Identities are used in various ways across Ash and it's extensions. This list is not necessarily exhaustive:

### Ash

* Identities can be used with `c:Ash.Api.get/3`, e.g `MyApi.get(User, [email: "foo@bar.com"])`

### AshPostgres

* The [migration generator](https://hexdocs.pm/ash_postgres/Mix.Tasks.AshPostgres.GenerateMigrations.html) creates unique constraints for identities

### AshJsonApi

* Get routes can be configured to use a specific identity, creating a route like `GET /users/foo@bar.com`

### AshGraphql

* Get queries and mutations can be configured to use a specific identity, to create a query like the following. (Arbitrary filtering is supported on list queries, this is is for creating queries that return a single result)

```graphql
query{
  getUser(email: "foo@bar.com"){
      id
  }
}
```
