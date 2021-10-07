# Getting Started Tutorial

This tutorial will walk you through creating a very simple application that uses
Ash. The finished application will look like this:
https://github.com/mario-mazo/my_app

## Creating an application

The first step is to [create an application](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).

```shell
mix new my_app
```

Note: alternatively you create a phoenix application with `mix phx.new` (which is covered in more detail in the [next guide](getting_started_phx.html)).

## Add Ash

Add `ash` to your dependencies in `mix.exs`. The latest version can be found by running `mix hex.info ash`.

```elixir
# in mix.exs
def deps() do
  [
    {:ash, "~> x.x.x"}
  ]
end
```

If you want to have a more idiomatic formatting (like the formatting used in the
documentation) of your Ash resource and APIs, you need to add `:ash` (and any other
extensions you use like `:ash_postgres`) to your `.formatter.exs` otherwise the
default Elixir formatter will wrap portions of the DSL in parenthesis.

```elixir
 import_deps: [
    :ash # add this line
  ]
```

Without that, instead of:

```elixir
attribute :id, :integer, allow_nil?: true
```

the Elixir formatter will change it to:

```elixir
attribute(:id, :integer, allow_nil?: true)
```

## Create an Ash API

Create an API module. This will be your primary way to interact with your Ash resources. We recommend `lib/my_app/api.ex` for simple setups.

```elixir
# lib/my_app/api.ex
defmodule MyApp.Api do
  use Ash.Api

  resources do
  end
end
```

## Create a registry

The registry is in charge of keeping track of the resources available to an api.

```elixir
# lib/my_app/registry.ex
defmodule MyApp.Registry do
  use Ash.Registry

  entries do
  end
end
```

## Refer to that registry in your api

```elixir
# lib/my_app/api.ex
defmodule MyApp.Api do
  use Ash.Api

  resources do
    registry MyApp.Registry
  end
end
```

## Create a resource

A resource is the primary entity in Ash. Your API module ties your resources together and gives them an interface, but the vast majority of your configuration will live in resources.

In your typical setup, you might have a resource per database table. For those already familiar with [Ecto](https://github.com/elixir-ecto/ecto), a resource and an Ecto schema are very similar. In fact, all resources define an Ecto schema under the hood. This can be leveraged when you need to do things that are not yet implemented or fall outside of the scope of Ash. The current recommendation for where to put your resources is in `lib/my_app/resources/<resource_name>.ex`. Here are a few examples:

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
    create_timestamp :inserted_at
    # This is updated on all updates
    update_timestamp :updated_at

    # `create_timestamp` above is just shorthand for:
    # attribute :inserted_at, :utc_datetime_usec,
    #   private?: true,
    #   writable?: false,
    #   default: &DateTime.utc_now/0
  end

end

# in lib/my_app/resources/user.ex
defmodule MyApp.User do
  use Ash.Resource

  attributes do
    attribute :email, :string,
      allow_nil?: false,
      constraints: [
        # Note: This regex is just an example
        match: ~r/^[\w.!#$%&’*+\-\/=?\^`{|}~]+@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*$/i
      ]

    uuid_primary_key :id
  end
end
```

For full details on defining a resource, see: `Ash.Resource.Dsl`.

## Add resources to your API

Alter your Registry (`lib/my_app/registry.ex`) to add the resources we created on the previous step:

```elixir
entries do
  entry MyApp.User
  entry MyApp.Tweet
end
```

### Test the resources

Now you are able to create changesets for your resources using `Ash.Changeset.new/2`:

```elixir
iex(7)> changeset = Ash.Changeset.new(MyApp.User, %{email: "ash.man@enguento.com"})
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
    email: nil,
    id: nil
  },
  valid?: true
>
```

If you try to use an invalid email (the email regex is for demonstration purposes only)
an error will be returned:

```elixir
iex(6)> changeset = Ash.Changeset.new(MyApp.User, %{email: "@eng.com"})
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
         regex: "~r/^[\\w.!#$%&‚Äô*+\\-\\/=?\\^`{|}~]+@[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*$/i"
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
    email: nil,
    id: nil
  },
  valid?: false
>
```

## Add your data layer

To be able to store and later on read your resources, a _data layer_ is required. For more information, see the documentation for the data layer you would like to use. The currently supported data layers are listed below:

| Storage  | Datalayer                                                | Storage Documentation                                                    |
| -------- | -------------------------------------------------------- | ------------------------------------------------------------------------ |
| postgres | [AshPostgres.DataLayer](https://hexdocs.pm/ash_postgres) | [Postgres Documentation](https://www.postgresql.org/docs/)               |
| csv      | [AshCsv.DataLayer](https://hexdocs.pm/ash_csv)           | [CSV Information](https://en.wikipedia.org/wiki/Comma-separated_values)  |
| ets      | `Ash.DataLayer.Ets`                                      | [Erlang Term Storage Documentation](https://erlang.org/doc/man/ets.html) |
| mnesia   | `Ash.DataLayer.Mnesia`                                   | [Mnesia Documentation](https://erlang.org/doc/man/mnesia.html)           |

To add a data layer, we need to add it to the `use Ash.Resource` statement. In
this case we are going to use ETS which is a in-memory data layer that is built
into the BEAM and works well for testing purposes.

```elixir
  # in both lib/my_app/resources/user.ex
  # and lib/my_app/resources/tweet.ex

  use Ash.Resource, data_layer: Ash.DataLayer.Ets
```

## Add actions to enable functionality

Actions are the primary driver for adding specific interactions to your resource.
You can read the about `Ash.Resource.Dsl` [actions](Ash.Resource.Dsl.html#module-actions)
to learn how to customize the functionality. For now we will enable all of them with default implementations by adding the following block to your resources:

```elixir
  # in both lib/my_app/resources/user.ex
  # and lib/my_app/resources/tweet.ex

  actions do
    create :create
    read :read
    update :update
    destroy :destroy
  end
```

### Test functionality

Now you should be able to use your API to do CRUD operations on your resources.

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
    email: nil,
    id: nil
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
   email: "ash.man@enguento.com",
   id: "2642ca11-330b-4a07-83c7-b0e9ef391df6"
 }}
```

##### List and Read a resource

```elixir
iex(3)> MyApp.Api.read(MyApp.User)
{:ok,
 [
   %MyApp.User{
     __meta__: #Ecto.Schema.Metadata<:built, "">,
     __metadata__: %{},
     aggregates: %{},
     calculations: %{},
     email: "ash.man@enguento.com",
     id: "2642ca11-330b-4a07-83c7-b0e9ef391df6"
   }
 ]}
iex(4)> MyApp.Api.get(MyApp.User, "2642ca11-330b-4a07-83c7-b0e9ef391df6")
{:ok,
 %MyApp.User{
   __meta__: #Ecto.Schema.Metadata<:built, "">,
   __metadata__: %{},
   aggregates: %{},
   calculations: %{},
   email: "ash.man@enguento.com",
   id: "2642ca11-330b-4a07-83c7-b0e9ef391df6"
 }}
```

## Add relationships

With our resources stored in a data layer we can move on
to create relationships between them. In this case we will
specify that a `User` can have many `Tweets` - this implies that
a `Tweet` belongs to a specific `User`.

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

Now we can use the new relationship to create a `Tweet` that belongs to a specific `User`:

```elixir
iex(8)> {:ok, user} = Ash.Changeset.new(MyApp.User, %{email: "ash.man@enguento.com"}) |> MyApp.Api.create()
{:ok,
 %MyApp.User{
   __meta__: #Ecto.Schema.Metadata<:built, "">,
   __metadata__: %{},
   aggregates: %{},
   calculations: %{},
   email: "ash.man@enguento.com",
   id: "0d7063f8-b07c-4d02-88b2-b671f1aa0ad9",
   tweets: #Ash.NotLoaded<:relationship>
 }}
iex(9)> MyApp.Tweet |> Ash.Changeset.new(%{body: "ashy slashy"}) |> Ash.Changeset.replace_relationship(:user, user) |> MyApp.Api.create()
{:ok,
 %MyApp.Tweet{
   __meta__: #Ecto.Schema.Metadata<:built, "">,
   __metadata__: %{},
   aggregates: %{},
   body: "ashy slashy",
   calculations: %{},
   inserted_at: ~U[2020-11-14 12:54:06Z],
   id: "f0b0b9d5-832c-45c9-9313-5e3fb9f1af24",
   public: false,
   updated_at: ~U[2020-11-14 12:54:06Z],
   user: %MyApp.User{
     __meta__: #Ecto.Schema.Metadata<:built, "">,
     __metadata__: %{},
     aggregates: %{},
     calculations: %{},
     email: "ash.man@enguento.com",
     id: "0d7063f8-b07c-4d02-88b2-b671f1aa0ad9",
     tweets: #Ash.NotLoaded<:relationship>
   },
   user_id: "0d7063f8-b07c-4d02-88b2-b671f1aa0ad9"
 }}
```

## Add a Phoenix Frontend

Now that the Elixir API is complete, you can move on to the [next
guide](getting_started_phx.html) to learn how to change the data_layer to
PostgreSQL and expose it via a JSON API.

- `AshJsonApi` - can be used to build a spec compliant JSON:API.
- `AshPostgres.DataLayer` - can be used to persist your resources to PostgreSQL.

## See Ash documentation for the rest

- `Ash.Api` for what you can do with your resources.
- `Ash.Query` for the kinds of queries you can make.
- `Ash.Resource.Dsl` for the resource DSL documentation.
- `Ash.Api.Dsl` for the API DSL documentation.
