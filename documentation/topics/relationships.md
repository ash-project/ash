# Relationships

Relationships are a core component of Ash. They provide a mechanism to describe the relationships between your resources, and through those relationships you can do things like

- Loading related data
- Filtering on related data
- Managing related records through changes on a single resource
- Authorizing based on the state of related data

## Customizing default belongs_to attribute type

By default, we assume foreign keys that we add by default (for `belongs_to` relationships) should be `:uuid`. To change this default, set the following configuration:

```elixir
config :ash, :default_belongs_to_type, :integer
```

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

### More complex data loading

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

## Managing related data

See [Managing Relationships](/documentation/topics/managing-relationships.md) for more information.


## Relationships Basics

All relationships have a `source` and a `destination`, as well as a corresponding `source_attribute` and `destination_attribute`. Many to many relationships have additional fields which are discussed below. Relationships will validate at compile time that their configured attributes exist. You don't need to have a corresponding "reverse" relationship for every relationship, i.e if you have a `MyApp.Tweets` resource with `belongs_to :user, User` you aren't required to have a `has_many :tweets, MyApp.Tweet`. All that is required is that the attributes used by the relationship exist.

## Kinds of relationships

### Belongs To

```elixir
belongs_to :owner, MyApp.User
```

A `belongs_to` relationship means that there is an attribute (`source_attribute`) on the source resource that uniquely identifies a record with a matching `destination_attribute` in the destination. In the example above, the source attribute would be `owner_id`, and if you wanted to change the owner, you'd modify the `owner_id` to point to a different `MyApp.User`

#### Belongs to Source Attribute

The `destination_attribute` defaults to `:id`.
By default, a belongs_to relationship will define an attribute called `<relationship_name>_id` of type `:uuid` on the resource. To configure this, use options like:

- `d:Ash.Resource.Dsl.relationships.belongs_to|define_attribute?` to define it yourself
- `d:Ash.Resource.Dsl.relationships.belongs_to|attribute_type` to modify the default type
- `d:Ash.Resource.Dsl.relationships.belongs_to|attribute_writable?` to make the source attribute `private?: false, writable?: true` (both are not the default)

For example:

```elixir
belongs_to :owner, MyApp.User do
  attribute_type :integer
  attribute_writable? true
end
```

Or if you wanted to define the attribute yourself,

```elixir
attributes do
  attribute :owner_foo, MyApp.CustomType
end

...
relationships do
  belongs_to :owner, MyApp.User do
    define_attribute? false
    source_attribute :owner_foo
  end
end
```

See the docs for more: `d:Ash.Resource.Dsl.relationships.belongs_to`

### Has One

```elixir
# on MyApp.User
has_one :profile, MyApp.Profile
```

A `has_one` is similar to a `belongs_to` except the "reference" attribute is on
the destination resource, instead of the source. In the example above, we'd expect a `profile_id` to be on `MyApp.Profile`, and that it is unique. 

#### Has One Attribute Defaults

By default, the `source_attribute` is assumed to be `:id`, and `destination_attribute` defaults to `<snake_cased_last_part_of_module_name>_id`. In the above example, it would default `destination_attribute` to `user_id`.

See the docs for more: `d:Ash.Resource.Dsl.relationships.has_one`

### Has Many

```elixir
# on MyApp.Post
has_many :comments, Comment
```

A `has_many` relationship is similar to a `has_one` in that the reference attribute is on the destination resource. The only difference between this and `has_one` is that it does not expect the destination attribute is unique on the destination, and therefore will produce a list of related items.

#### Has Many Attribute Defaults

By default, the `source_attribute` is assumed to be `:id`, and `destination_attribute` defaults to `<snake_cased_last_part_of_module_name>_id`. In the above example, it would default `destination_attribute` to `post_id`.

See the docs for more: `d:Ash.Resource.Dsl.relationships.has_many`
## Many To Many Relationships

Lets say that individual todo items in our app can be added to multiple lists, and every list has multiple todo items. This is a great case for `many_to_many` relationships.

For example, we could define the following `many_to_many` relationship:

```elixir
# on MyApp.TodoList
many_to_many :todo_items, MyApp.TodoItem do
  through MyApp.TodoListItem
  source_attribute_on_join_resource :list_id
  destination_attribute_on_join_resource :item_id
end
```

And then we could define the "join resource" to connect everything: `MyApp.TodoListItem`

```elixir
defmodule MyApp.TodoListItem do
  use Ash.Resource,
    data_layer: your_data_layer

  attributes do
    uuid_primary_key :id
  end

  relationships do
    belongs_to :todo_list, MyApp.TodoList do
      allow_nil? false
    end

    belongs_to :item, MyApp.TodoItem do
      allow_nil? false
    end
  end
end
```

Now that we have a resource with the proper attributes, Ash will use this automatically under the hood when 
performing the relationship operations detailed above, like filtering and loading.

See the docs for more: `d:Ash.Resource.Dsl.relationships.many_to_many`