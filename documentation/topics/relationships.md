# Relationships

Relationships are a core component of Ash. They provide a mechanism to describe the relationships between your resources, and through those relationships you can do things like

- Loading related data
- Filtering on related data
- Managing related records through changes on a single resource
- Authorizing based on the state of related data

## Managing related data

See {{link:ash:guide:Managing Relationships}} for more information.

## Loading related data

Loading relationships is a very common use case. There are two ways to load relationships, in the query, and on records.

### On records

Given a set of records, like `[user1, user2]`, you can load their relationships by calling your Ash Api's `load` function.

```elixir
YourApi.load(users, :friends)
```

This will fetch the friends of each user, and set them in the corresponding `friends` key.

### In the query

Loading in the query is currently pretty much the same as loading on records, but eventually data layers will be able to optimize these loads, potentially including them as joins in the main query, for example. The following will return the list of users with their friends loaded, as the above example.

```elixir
User
|> Ash.Query.load(:friends)
|> YourApi.read()
```

## More complex data loading

Multiple relationships can be loaded at once, i.e

```elixir
YourApi.load(users, [:friends, :enemies])
```

Nested relationships can be loaded:

```elixir
YourApi.load(users, friends: [:friends, :enemies])
```

The queries used for loading can be customized by providing a query as the value.

```elixir
friends = Ash.Query.sort(User, social_score: :asc)

YourApi.load(users, friends: friends)
```

Nested loads will be included in the parent load.

```elixir
friends = 
  User
  |> Ash.Query.sort(social_score: :asc)
  |> Ash.Query.load(:friends)

# Will load friends and friends of those friends
YourApi.load(users, friends: friends)
```