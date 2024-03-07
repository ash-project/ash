# Relationships

Relationships describe the connections between resources and are a core component of Ash. Defining relationships enables you to do things like

- Loading related data
- Filtering on related data
- Managing related records through changes on a single resource
- Authorizing based on the state of related data

## Relationships Basics

A relationship exists between a source resource and a destination resource. These are defined in the `relationships` block of the source resource. For example, if `MyApp.Tweet` is the source resource, and `MyApp.User` is the destination resource, we could define a relationship called `:owner` like this:

```elixir
defmodule MyApp.Tweet do
  use Ash.Resource,
    data_layer: my_data_layer

  attributes do
    uuid_primary_key :id
    attribute :body, :string
  end

  relationships do
    belongs_to :owner, MyApp.User
  end
end
```

## Managing related data

See [Managing Relationships](/documentation/topics/managing-relationships.md) for more information.

Your data layer may enforce foreign key constraints, see the following guides for more information:

- [AshPostgres references](https://hexdocs.pm/ash_postgres/references.html)

## Kinds of relationships

There are four kinds of relationships:

- [`belongs_to`](#belongs-to)
- [`has_one`](#has-one)
- [`has_many`](#has-many)
- [`many_to_many`](#many-to-many)

Each of these relationships has a `source` resource and a `destination` resource with a corresponding attribute on the source resource (`source_attribute`), and destination resource (`destination_attribute`). Relationships will validate that their configured attributes exist at compile time.

You don't need to have a corresponding "reverse" relationship for every relationship, i.e if you have a `MyApp.Tweet` resource with `belongs_to :user, MyApp.User` you aren't required to have a `has_many :tweets, MyApp.Tweet` on `MyApp.User`. All that is required is that the attributes used by the relationship exist.

### Belongs To

```elixir
# on MyApp.Tweet
belongs_to :owner, MyApp.User
```

A `belongs_to` relationship means that there is an attribute (`source_attribute`) on the source resource that uniquely identifies a record with a matching attribute (`destination_attribute`) in the destination. In the example above, the source attribute on `MyApp.Tweet` is `:owner_id` and the destination attribute on `MyApp.User` is `:id`.

#### Attribute Defaults

By default, the `source_attribute` is defined as `:<relationship_name>_id` of the type `:uuid` on the source resource and the `destination_attribute` is assumed to be `:id`. You can override the attribute names by specifying the `source_attribute` and `destination_attribute` options like so:

```elixir
belongs_to :owner, MyApp.User do
  # defaults to :<relationship_name>_id (i.e. :owner_id)
  source_attribute :custom_attribute_name

  # defaults to :id
  destination_attribute :custom_attribute_name
end
```

You can further customize the `source_attribute` using options such as:

- `d:Ash.Resource.Dsl.relationships.belongs_to|define_attribute?` to define it yourself
- `d:Ash.Resource.Dsl.relationships.belongs_to|attribute_type` to modify the default type
- `d:Ash.Resource.Dsl.relationships.belongs_to|attribute_public?` to make the source attribute `public?: true`

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

#### Customizing default belongs_to attribute type

Destination attributes that are added by default are assumed to be `:uuid`. To change this, set the following configuration in `config.exs`:

```elixir
config :ash, :default_belongs_to_type, :integer
```

See the docs for more: `d:Ash.Resource.Dsl.relationships.belongs_to`

### Has One

```elixir
# on MyApp.User
has_one :profile, MyApp.Profile
```

A `has_one` relationship means that there is a unique attribute (`destination_attribute`) on the destination resource that identifies a record with a matching unique attribute (`source_resource`) in the source. In the example above, the source attribute on `MyApp.User` is `:id` and the destination attribute on `MyApp.Profile` is `:user_id`.

A `has_one` is similar to a `belongs_to` except the reference attribute is on
the destination resource, instead of the source.

#### Attribute Defaults

By default, the `source_attribute` is assumed to be `:id`, and `destination_attribute` defaults to `<snake_cased_last_part_of_module_name>_id`.

See the docs for more: `d:Ash.Resource.Dsl.relationships.has_one`

### Has Many

```elixir
# on MyApp.User
has_many :tweets, MyApp.Tweet
```

A `has_many` relationship means that there is a non-unique attribute (`destination_attribute`) on the destination resource that identifies a record with a matching unique attribute (`source_resource`) in the source. In the example above, the source attribute on `MyApp.User` is `:id` and the destination attribute on `MyApp.Tweet` is `:user_id`.

A `has_many` relationship is similar to a `has_one` because the reference attribute exists on the destination resource. The only difference between this and `has_one` is that the destination attribute is not unique, and therefore will produce a list of related items. In the example above, `:tweets` corresponds to a list of `MyApp.Tweet` records.

#### Attribute Defaults

By default, the `source_attribute` is assumed to be `:id`, and `destination_attribute` defaults to `<snake_cased_last_part_of_module_name>_id`.

See the docs for more: `d:Ash.Resource.Dsl.relationships.has_many`

### Many To Many

A `many_to_many` relationship can be used to relate many source resources to many destination resources. To achieve this, the `source_attribute` and `destination_attribute` are defined on a join resource. A `many_to_many` relationship can be thought of as a combination of a `has_many` relationship on the source/destination resources and a `belongs_to` relationship on the join resource.

For example, consider two resources `MyApp.Tweet` and `MyApp.Hashtag` representing tweets and hashtags. We want to be able to associate a tweet with many hashtags, and a hashtag with many tweets. To do this, we could define the following `many_to_many` relationship:

```elixir
# on MyApp.Tweet
many_to_many :hashtags, MyApp.Hashtag do
  through MyApp.TweetHashtag
  source_attribute_on_join_resource :tweet_id
  destination_attribute_on_join_resource :hashtag_id
end
```

The `through` option specifies the "join" resource that will be used to store the relationship. We need to define this resource as well:

```elixir
defmodule MyApp.TweetHashtag do
  use Ash.Resource,
    data_layer: your_data_layer

  postgres do
    table "tweet_hashtags"
    repo MyApp.Repo
  end

  relationships do
    belongs_to :tweet, MyApp.Tweet, primary_key?: true, allow_nil?: false
    belongs_to :hashtag, MyApp.Hashtag, primary_key?: true, allow_nil?: false
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end
end
```

It is convention to name this resource `<source_resource_name><destination_resource_name>` however this is not required. The attributes on the join resource must match the `source_attribute_on_join_resource` and `destination_attribute_on_join_resource` options on the `many_to_many` relationship. The relationships on the join resource are standard `belongs_to` relationships, and can be configured as such. In this case, we have specified that the `:tweet_id` and `:hashtag_id` attributes form the primary key for the join resource, and that they cannot be `nil`.

Now that we have a resource with the proper attributes, Ash will use this automatically under the hood when
performing relationship operations like filtering and loading.

See the docs for more: `d:Ash.Resource.Dsl.relationships.many_to_many`

### Cross-domain relationships

You will need to specify the `domain` option in the relationship if the destination resource and/or the join table are parts of a different domain:

```elixir
many_to_many :authors, MyApp.OtherDomain.Resource do
  domain MyApp.OtherDomain
  ...
end
```

However, if the join table is a part of the same domain but the destination resource is a part of a different domain, you will have to add a custom `has_many` association to the source domain as well. Suppose you have an domain called `MyApp.Organizations` with a resource named `MyApp.Organizations.Organization`, an domain called `MyApp.Accounts` with a resource named `MyApp.Accounts.User`, as well as a join resource `MyApp.Organizations.OrganizationUsers`. Then, in order to add `many_to_many :users` for `MyApp.Organizations.Organization`, you'll need the following setup:

```elixir
relationships do
  ...

  has_many :users_join_assoc, MyApp.Organizations.OrganizationUsers

  many_to_many :users, MyApp.Accounts.User do
    domain MyApp.Accounts
    through MyApp.Organizations.OrganizationUsers
    source_attribute_on_join_resource :organization_id
    destination_attribute_on_join_resource :user_id
  end
end
```

## Loading related data

There are two ways to load relationships:

- in the query using `Ash.Query.load/2`
- directly on records using `Ash.load/3`

### On records

Given a single record or a set of records, it is possible to load their relationships by calling the `load` function on the record's parent domain. For example:

```elixir
# user = %User{...}
Ash.load(user, :tweets)

# users = [%User{...}, %User{...}, ....]
Ash.load(users, :tweets)
```

This will fetch the tweets for each user, and set them in the corresponding `tweets` key.

```elixir
%User{
  ...
  tweets: [
    %Tweet{...},
    %Tweet{...},
    ...
  ]
}
```

See `Ash.load/3` for more information.

### In the query

The following will return a list of users with their tweets loaded identically to the previous example:

```elixir
User
|> Ash.Query.load(:tweets)
|> Ash.read()
```

At present, loading relationships in the query is fundamentally the same as loading on records. Eventually, data layers will be able to optimize these loads (potentially including them as joins in the main query).

See `Ash.Query.load/2` for more information.

### More complex data loading

Multiple relationships can be loaded at once, i.e

```elixir
Ash.load(users, [:tweets, :followers])
```

Nested relationships can be loaded:

```elixir
Ash.load(users, followers: [:tweets, :followers])
```

The queries used for loading can be customized by providing a query as the value.

```elixir
followers = Ash.Query.sort(User, follower_count: :asc)

Ash.load(users, followers: followers)
```

Nested loads will be included in the parent load.

```elixir
followers =
  User
  |> Ash.Query.sort(follower_count: :asc)
  |> Ash.Query.load(:followers)

# Will load followers and followers of those followers
Ash.load(users, followers: followers)
```

## no_attributes? true

This can be very useful when combined with multitenancy. Specifically, if you have a tenant resource like `Organization`,
you can use `no_attributes?` to do things like `has_many :employees, Employee, no_attributes?: true`, which lets you avoid having an
unnecessary `organization_id` field on `Employee`. The same works in reverse: `has_one :organization, Organization, no_attributes?: true`
allows relating the employee to their organization.

Some important caveats here:

1. You can still manage relationships from one to the other, but "relate" and "unrelate"
will have no effect, because there are no fields to change.

2. Loading the relationship on a list of resources will not behave as expected in all circumstances involving multitenancy. For example,
   if you get a list of `Organization` and then try to load `employees`, you would need to set a single tenant on the load query, meaning
   you'll get all organizations back with the set of employees from one tenant. This could eventually be solved, but for now it is considered an
   edge case.
