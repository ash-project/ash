# Getting Started

## Creating an application

For information on creating a new Elixir application, see [this guide](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)

## Add Ash

Add `ash` to your dependencies in `mix.exs`. The latest version can be found by running `mix hex.info ash`.

## Create an Ash API

Create an API module. This will be your primary way to interact with your Ash resources. We recommend `lib/api.ex` for simple setups.

```elixir
defmodule MyApp.Api do
  use Ash.Api

  resources do
  end
end
```

Then, add `MyApp.Api` to your `application.ex`'s start function, which should look something like this:

```elixir
def start(_type, _args) do
  children = [
    # Start the Ecto repository
    MyApp.Repo,
    # Start the Telemetry supervisor
    MyApp.Telemetry,
    # Start the PubSub system
    {Phoenix.PubSub, name: MyApp.PubSub},
    # Start the Endpoint (http/https)
    MyApp.Endpoint,
    MyApp.Api # <- Add your API here
  ]
   ...
end
```

## Create a resource

A resource is the primary entity in Ash. Your Api module ties your resources together and gives them an interface, but the vast majority if your configuration will live in a resource. In your typical setup, you might have a resource per database table. For those already familiar with ecto, a resource and an ecto schema are very similar. In fact, all resources define an ecto schema under the hood. This can be leveraged when you need to do things that are not yet implemented or fall outside of the scope of Ash. Here are a few examples:

```elixir
defmodule MyApp.Tweet do
  use Ash.Resource

  attributes do
    attribute :id, :uuid do
      # All ash resources currently require a primary key
      # Eventually, we will add good defaults and/or allow
      # for a global configuration of your default primary key
      primary_key? true
      allow_nil? false
      writable? false
      default &Ecto.UUID.generate/0
    end

    attribute :body, :string do
      allow_nil? false
      constraints [max_length: 255]
    end

    # Alternatively, you can use the keyword list syntax
    # `{:constant, <value>}` is how you set a default
    # You can also set functional defaults, via passing in a zero
    # argument function or an MFA
    attribute :public, :boolean, allow_nil?: false, default: {:constant, false}

    create_timestamp :created_at #This is set on create
    update_timestamp :updated_at #This is updated on all updates

    # `create_timestamp` above is just shorthand for:
    attribute :created_at, :utc_datetime, writable?: false, default: &DateTime.utc_now/0
  end

  relationships do
    belongs_to :user, MyApp.User
  end
end

defmodule MyApp.User do
  use Ash.Resource

  attributes do
    attribute :email, :string, allow_nil?: false, constraints: [
      match: ~r/[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}/
    ]
  end

  relationships do
    has_many :tweets, MyApp.Tweet, destination_field: :user_id
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

## Add your datalayer

Choose a datalayer, and see its documentation for configuring it:

- `Ash.DataLayer.Ets` - an [ets](https://erlang.org/doc/man/ets.html) datalayer only recommended for testing
- `Ash.DataLayer.Mnesia` - an [mnesia](https://erlang.org/doc/man/mnesia.html) datalayer, not optimized, but is backed by a file and works with distributed applications
- `AshPostgres.DataLayer` - a Postgres datalayer, currently the primary supported data layer

To add a datalayer, add it to the `use Ash.Resource` statement:

```elixir
use Ash.Resource,
  data_layer: AshPostgres.DataLayer
```

## Add actions to enable functionality

Currently, actions do not offer any customization, but eventually they will be the primary driver for adding specific interactions to your resource. For now, to enable all of them, add the following to your resource:

```elixir
actions do
  create :default
  read :default
  update :default
  destroy :default
end
```

## See Ash documentation for the rest

- `Ash.Api` for what you can do with your resources.
- `Ash.Query` for the kinds of queries you can make.
- `Ash.Dsl` for the resource DSL documentation.
- `Ash.Api.Dsl` for the API DSL documentation.
