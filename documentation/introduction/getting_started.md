# Getting Started

## Creating an application

For information on creating a new Elixir application, see [this guide](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)

```shell
mix new my_app
```

## Add Ash

Add `ash` to your dependencies in `mix.exs`. The latest version can be found by running `mix hex.info ash`.

```elixir
# in mix.exs
def deps() do
  [
    {:ash, "~> x.x.x"},
  ]
end
```

If you want to have a more idiomating formating (the one used in this documentation) of you code files
you need to add to your `.formatter.exs` otherwise the default elixir formater with include `(` and `)`
acording to default `mix format` rules.

```elixir
 import_deps: [
    :ash
  ],
```


## Phoenix

If you intend to use `ash_json_api` or `ash_graphql`, you will likely want to create a new phoenix application.
Phoenix provides a lot of extra capabilities at very little cost, and can be a very useful escape hatch if you need to
add something to your application that isn't supported by Ash yet. See the [phoenix](https://www.phoenixframework.org/) documentation for creating
a new phoenix application. Ultimately, instead of `mix new my_application`, you would use:

```shell
mix phx.new my_application
```

## Create an Ash API

Create an API module. This will be your primary way to interact with your Ash resources. We recommend `lib/my_app/api.ex` for simple setups. For more information on organizing resources into contexts/domains, see the "Contexts and Domains" guide.

```elixir
# lib/my_app/api.ex
defmodule MyApp.Api do
  use Ash.Api

  resources do
  end
end
```

## Create a resource

A resource is the primary entity in Ash. Your Api module ties your resources together and gives them an interface, but the vast majority if your configuration will live in a resource. In your typical setup, you might have a resource per database table. For those already familiar with ecto, a resource and an ecto schema are very similar. In fact, all resources define an ecto schema under the hood. This can be leveraged when you need to do things that are not yet implemented or fall outside of the scope of Ash. The current recommendation for where to put your resources is in `lib/my_app/resources/<resource_name>.ex`. Here are a few examples:

```elixir
# in lib/my_app/resources/tweet.ex
defmodule MyApp.Tweet do
  use Ash.Resource

  attributes do
    uuid_primary_key :id

    attribute :body, :string do
      allow_nil? false
      constraints max_length: 255
    end

    # Alternatively, you can use the keyword list syntax
    # You can also set functional defaults, via passing in a zero
    # argument function or an MFA
    attribute :public, :boolean, allow_nil?: false, default: false

    # This is set on create
    create_timestamp :created_at
    # This is updated on all updates
    update_timestamp :updated_at

    # `create_timestamp` above is just shorthand for:
    # attribute(:created_at, :utc_datetime, writable?: false, default: &DateTime.utc_now/0)
  end

end

# in lib/my_app/resources/user.ex
defmodule MyApp.User do
  use Ash.Resource

  attributes do
    attribute :email, :string,
      allow_nil?: false,
      constraints: [
        match: ~r/^[\w.!#$%&’*+\-\/=?\^`{|}~]+@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*$/i
      ],
      primary_key?: true
    attribute :id, :uuid, default: &Ecto.UUID.generate/0
  end
end
```

## Add resources to your API

Alter your API like so:

```elixir
resources do
  resource MyApp.User
  resource MyApp.Tweet
end
```

### Test the resources

Now you should be able to create changesets for the resources

```elixir
iex(7)> change = Ash.Changeset.new(MyApp.User, %{email: "ash.man@enguento.com"})
#Ash.Changeset<
  action_type: :create,
  attributes: %{email: "ash.man@enguento.com"},
  relationships: %{},
  errors: [],
  data: %MyApp.User{
    __meta__: #Ecto.Schema.Metadata<:built, "">,
    __metadata__: %{},
    aggregates: %{},
    calculations: %{},
    email: nil
  },
  valid?: true
>
```

If you try to use a invalid email(The email regex is for demostration purposes only)
an error will be displayed as shown

```elixir
iex(6)> change = Ash.Changeset.new(MyApp.User, %{email: "@eng.com"})
#Ash.Changeset<
  action_type: :create,
  attributes: %{},
  relationships: %{},
  errors: [
    %Ash.Error.Changes.InvalidAttribute{
      class: :invalid,
      field: :email,
      message: {"must match the pattern %{regex}",
       [
         regex: "~r/^[\\w.!#$%&‚Äö√Ñ√¥*+\\-\\/=?\\^`{|}~]+@[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)
*$/i"
       ]},
      path: [],
      stacktrace: #Stacktrace<>
    }
  ],
  data: %MyApp.User{
    __meta__: #Ecto.Schema.Metadata<:built, "">,
    __metadata__: %{},
    aggregates: %{},
    calculations: %{},
    email: nil
  },
  valid?: false
>
```

## Add your data_layer

To be able to store and later on read those resources a _data layer_ is
needed you can choose a `data_layer`, and see its documentation for configuring it:

- `Ash.DataLayer.Ets` - an [ets](https://erlang.org/doc/man/ets.html) data_layer only recommended for testing
- `Ash.DataLayer.Mnesia` - an [mnesia](https://erlang.org/doc/man/mnesia.html) data_layer, not optimized, but is backed by a file and works with distributed applications
- `AshPostgres.DataLayer` - a Postgres data_layer, currently the primary supported data layer

To add a `data_layer`, add it to the `use Ash.Resource` statement. In this
case we are going to use `ETS` which is a in memory data layer good enough
for testing purposes. Also we will make the ETS private so Read/Write limited
to owner process.

```elixir
  # in both lib/my_app/resources/user.ex
  # and lib/my_app/resources/tweet.ex

  use Ash.Resource, data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

```

## Add actions to enable functionality

Actions are the primary driver for adding specific interactions to your resource.
You can read the [actions](https://hexdocs.pm/ash/Ash.Resource.Dsl.html#actions/1
) section to learn to to customze the functionality
for now we will enable all of them with a default implementations by adding
following to your resource:
```elixir
  # in both lib/my_app/resources/user.ex
  # and lib/my_app/resources/tweet.ex

  actions do
    create :default
    read :default
    update :default
    destroy :default
  end
```

### Test functionality

Now you should be able to use you API to do CRUD operations in your resources

#### Create resource

```elixir
iex(1)> user_changeset = Ash.Changeset.new(MyApp.User, %{email: "ash.man@enguento.co
m"})
#Ash.Changeset<
  action_type: :create,
  attributes: %{email: "ash.man@enguento.com"},
  relationships: %{},
  errors: [],
  data: %MyApp.User{
    __meta__: #Ecto.Schema.Metadata<:built, "">,
    __metadata__: %{},
    aggregates: %{},
    calculations: %{},
    email: nil
  },
  valid?: true
>
iex(2)> MyApp.Api.create(user_changeset)
{:ok,
 %MyApp.User{
   __meta__: #Ecto.Schema.Metadata<:built, "">,
   __metadata__: %{},
   aggregates: %{},
   calculations: %{},
   email: "ash.man@enguento.com"
 }}
 ```

##### List and Read a resouce

```elixir
iex(3)> MyApp.Api.read MyApp.User
{:ok,
 [
   %MyApp.User{
     __meta__: #Ecto.Schema.Metadata<:built, "">,
     __metadata__: %{},
     aggregates: %{},
     calculations: %{},
     email: "ash.man@enguento.com"
   }
 ]}
iex(4)> MyApp.Api.get(MyApp.User, "ash.man@enguento.com")
{:ok,
 %MyApp.User{
   __meta__: #Ecto.Schema.Metadata<:built, "">,
   __metadata__: %{},
   aggregates: %{},
   calculations: %{},
   email: "ash.man@enguento.com"
 }}

```

## Add relationships

Now with our resouces stored in a data layer we can move on
to credate a relationship between them. In this case we will
specify that a `User` can have many `Tweet` this implies that
a twee belongs to a specific user.

```elixir
# in lib/my_app/resources/user.ex
  relationships do
    has_many :tweets, MyApp.Tweet, destination_field: :user_id
  end

# in lib/my_app/resources/tweet.ex
  relationships do
    belongs_to :user, MyApp.User
  end
```

### Test relationships
## Add front end extensions

- `AshJsonApi` - can be used to build a spec compliant JSON:API

## See Ash documentation for the rest

- `Ash.Api` for what you can do with your resources.
- `Ash.Query` for the kinds of queries you can make.
- `Ash.Resource.Dsl` for the resource DSL documentation.
- `Ash.Api.Dsl` for the API DSL documentation.
