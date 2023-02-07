# Get Started

<!--- ash-hq-hide-start --> <!--- -->

This documentation is best viewed at [ash-hq.org](https://ash-hq.org)

<!--- ash-hq-hide-stop --> <!--- -->

## Goals

In this guide we will:

1. Create a new Phoenix application and add Ash, AshPhoenix and AshPostgres as dependencies
2. Create a simple set of resources and show they can be used
3. Illustrate some core concepts of Ash
4. Point you to good next resources so you can explore Ash further

## Things you may want to read first

- [Install Elixir](https://elixir-lang.org/install.html)
- [Phoenix - Up and Running Guide](https://hexdocs.pm/phoenix/up_and_running.html)
- [Philosophy Guide](/documentation/tutorials/philosophy.md)

## Requirements

If you want to follow along yourself, you will need the following things:

1. Elixir (1.12 or later) and Erlang (22 or later) installed
2. PostgreSQL installed
3. A text editor to make the changes that we make
4. A terminal to run the examples

## Steps

For this tutorial, we'll use examples based around creating a help desk.

We will make the following resources:

- `PhoenixHelpdesk.Support.Ticket`
- `PhoenixHelpdesk.Support.Representative`

The actions we will be able to take on these resources include:

- Opening a new Ticket
- Closing a Ticket
- Assigning a Ticket to a representative

### Create a New Phoenix Project

_This section is based the [Phoenix installation docs](https://hexdocs.pm/phoenix/installation.html). For more details go there._

We first need to create a fresh phoenix project using the phoenix project generator.

```bash
mix archive.install hex phx_new
```

And then use the project generator.

```bash
mix phx.new phoenix_helpdesk
```

This will then generate a project boilerplate and ask if you want to fetch and install dependencies. **Answer 'Yes' to 'Fetch and install dependencies? [Yn]'**.

Your terminal should now look very similar to this:

```bash
* creating phoenix_helpdesk/priv/static/images/phoenix.png
* creating phoenix_helpdesk/priv/static/favicon.ico

Fetch and install dependencies? [Yn] y
* running mix deps.get
* running mix deps.compile

We are almost there! The following steps are missing:

    $ cd phoenix_helpdesk

Then configure your database in config/dev.exs and run:

    $ mix ecto.create

Start your Phoenix app with:

    $ mix phx.server

You can also run your app inside IEx (Interactive Elixir) as:

    $ iex -S mix phx.server
```

Please `cd` into the project directory **BUT DO NOT RUN `mix ecto.create`.** We first need to integrate Ash and AshPostgres into our project.

### Add Dependencies

We now need to add Ash, AshPhoenix and AshPostgres to our Phoenix project.

First lets add the dependencies to the `deps` function in our `mix.exs`:

```elixir
# mix.exs

  def deps do
    [
      {:phoenix, "~> 1.6.15"},
      # ...
      {:ash, "~> 2.5.10"},
      {:ash_postgres, "~> 1.3.6"},
      {:ash_phoenix, "~> 1.1"},
      # If using ElixirLS then including elixir_sense enabled ash auto-complete
      {:elixir_sense, github: "elixir-lsp/elixir_sense", only: [:dev, :test]}
    ]
  end
```

Each Ash package also has its own formatter extension, include these in your `.formatter.exs` to ensure consistent formatting.

```elixir
# .formatter.exs
[
  import_deps: [:ecto, :phoenix, :ash, :ash_phoenix, :ash_postgres],
  inputs: ["*.{ex,exs}", "priv/*/seeds.exs", "{config,lib,test}/**/*.{ex,exs}"],
  subdirectories: ["priv/*/migrations"]
]
```

Now in the terminal install these new dependencies:

```bash
mix deps.get
```

### Swap `Ecto.Repo` for `AshPostgres.Repo`

Change your repo module to look like this:

```elixir
# lib/phoenix_helpdesk/repo.ex

defmodule PhoenixHelpdesk.Repo do
  use AshPostgres.Repo, opt_app: :phoenix_helpdesk

  def installed_extensions do
    ["uuid-ossp", "citext"]
  end
end
```

`AshPostgres.Repo` enriches your repo with additional AshPostgres specific behaviour, but is essentially a thin wrapper around `Ecto.Repo`.

### Edit Config

We need to specify the apis that ash uses and point to the Ecto repo we just changes into an AshPostgres repo.

```elixir
import Config

# For backwards compatibility, the following configuration is required.
# see https://ash-hq.org/docs/guides/ash/latest/get-started#temporary-config for more details
config :ash, :use_all_identities_in_manage_relationship?, false

config :phoenix_helpdesk,
  ash_apis: [PhoenixHelpdesk.Support]

config :phoenix_helpdesk,
  ecto_repos: [PhoenixHelpdesk.Repo]
```

### Create API

You may have noticed that `PhoenixHelpdesk.Support` doesn't actually exist yet. Lets make it now.

```elixir
# lib/phoenix_helpdesk/support.ex

defmodule PhoenixHelpdesk.Support do
  use Ash.Api

  resources do
    registry PhoenixHelpdesk.Support.Registry
  end
end
```

This tiny module tells Ash that`PhoenixHelpdesk.Support` is an Ash API. It can be thought of as a [Bounded Context](https://martinfowler.com/bliki/BoundedContext.html) in Domain Driven Design terms and can seen as analogous to a Phoenix context.

It also tells where to find the resources within this API - in the `PhoenixHelpdesk.Support.Registry` which we will make now.

### Create Registry

```elixir
# lib/phoenix_helpdesk/support/registry.ex

defmodule PhoenixHelpdesk.Support.Registry do
  use Ash.Registry,
    extensions: [
      # This extension adds helpful compile time validations
      Ash.Registry.ResourceValidations
    ]

  entries do
    entry PhoenixHelpdesk.Support.Ticket
    entry PhoenixHelpdesk.Support.Representative
  end
end
```

In the registry we enumerate the Ash resources we want our Ash API to contain. We're going to have two Resources. `Ticket` and `Representative`.

### Write `Ticket` Ash Resource

Below is the `Ticket` Ash resource. Please read the comments carefully, it describes what each line does:

```elixir
# lib/helpdesk/support/resources/ticket.ex

defmodule PhoenixHelpdesk.Support.Ticket do

  # Using Ash.Resource here turns this model into an Ash resource.
  use Ash.Resource,
    # Tells ash you want this resource to store its data in postgres.
    data_layer: AshPostgres.DataLayer

  # The postgres keyword is specific to the AshPostgres module.
  # It tells postgres what to call the table and to
  # communicate with postgres through PhoenixHelpdesk.Repo
  postgres do

    # Tells postgres what to call the table
    table "tickets"
    # Tells Ash how to interface with the postgres table
    repo PhoenixHelpdesk.Repo
  end

  actions do
    # exposes default built in actions to modify the resource
    defaults [:create, :read, :update, :destroy]

    # This is a custom create action which only accepts the subject parameter.
    # By default you can provide all public attributes to an action
    create :open do
      accept [:subject]
    end

    # This is a custom update action which accepts no parameters.
    update :close do
      # We don't want to accept any input here
      accept []

      # sets the attribute `:status` to `:closed`
      change set_attribute(:status, :closed)
    end

    update :assign do
      # No attributes should be accepted
      accept []

      # We accept a representative's id as input here
      argument :representative_id, :uuid do
        # This action requires representative_id
        allow_nil? false
      end

      # We use a change here to replace the related Representative
      # If there is a different representative for this Ticket, it will be changed to the new one
      # The Representative itself is not modified in any way
      change manage_relationship(:representative_id, :representative, type: :append_and_remove)
    end
  end

  # Attributes are the simple pieces of data that exist on your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id
    # Add a string type attribute called `:subject`
    attribute :subject, :string do
      # We don't want subject to ever be `nil`
      allow_nil? false
    end

    # status is either `open` or `closed`.
    attribute :status, :atom do
      # Constraints allow you to provide extra rules for the value.
      # The available constraints depend on the type
      # See the documentation for each type to know what constraints are available
      # Since atoms are generally only used when we know all of the values
      # it provides a `one_of` constraint, that only allows those values
      constraints [one_of: [:open, :closed]]

      # The status defaulting to open makes sense
      default :open
      # We also don't want status to ever be `nil`
      allow_nil? false
    end
  end

  relationships do
    # belongs_to means that the destination attribute is unique, meaning only one related record could exist.
    # We assume that the destination attribute is `representative_id` based
    # on the name of this relationship and that the source attribute is `representative_id`.
    # We create `representative_id` automatically.
    belongs_to :representative, PhoenixHelpdesk.Support.Representative
  end
end
```

### Write `Representative` Ash Resource

This is the `Representative` Ash resource.

```elixir
# lib/phoenix_helpdesk/support/resources/representative.ex

defmodule PhoenixHelpdesk.Support.Representative do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "representatives"
    repo PhoenixHelpdesk.Repo
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string
  end

  relationships do
    # `has_many` means that the destination attribute is not unique, therefore many related records could exist.
    # We assume that the destination attribute is `representative_id` based
    # on the module name of this resource and that the source attribute is `id`.
    has_many :tickets, PhoenixHelpdesk.Support.Ticket
  end
end
```

### Create and Migrate Database

We have specified the resource attributes, actions and relationships in Ash. But we have yet to create them in the data layer.

We do this by creating our database. And then generating and performing migration. If you have used Ecto before then its like that but much easier.

First lets create our database:

```bash
> mix ash_postgres.create

The database for PhoenixHelpdesk.Repo has been created
```

Then in our CLI we need to generate a migration like so:

```bash
> mix ash_postgres.generate_migrations --name initial_migration
Compiling 18 files (.ex)
Generated phoenix_helpdesk app

Extension Migrations:
* creating priv/resource_snapshots/extensions.json
* creating priv/repo/migrations/20230207084556_install_2_extensions.exs

Generating Tenant Migrations:

Generating Migrations:
* creating priv/repo/migrations/20230207084558_initial_migration.exs
```

The file we are concerned with now is `20230207084558_initial_migration.exs`. If we open it up we should see an `up/0` function which should look something like this:

```elixir
def up do
    # creates the tickets table and all of its attributes
    create table(:tickets, primary_key: false) do
      add :id, :uuid, null: false, primary_key: true
      add :subject, :text, null: false
      add :status, :text, null: false, default: "open"
      # this is here because of the one to many
      # relationship representatives have with tickets
      add :representative_id, :uuid
    end

    # creates the `:representatives` table
    create table(:representatives, primary_key: false) do
      add :id, :uuid, null: false, primary_key: true
    end

    # links the `:id` in the `:representatives` table with the
    # `:representatives_id` in the `:tickets` table.
    alter table(:tickets) do
      modify :representative_id,
             references(:representatives,
               column: :id,
               name: "tickets_representative_id_fkey",
               type: :uuid
             )
    end

    # adds the name column to the `:representative` table
    alter table(:representatives) do
      add :name, :text
    end
  end

```

We can run this function which will run this up function and perform these operations on the postgres database. We Run this command:

```bash
> mix ash_postgres.migrate

18:06:12.567 [info] == Running 20230207084556 PhoenixHelpdesk.Repo.Migrations.Install2Extensions.up/0 forward

18:06:12.574 [info] execute "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

18:06:12.616 [info] execute "CREATE EXTENSION IF NOT EXISTS \"citext\""

18:06:12.659 [info] == Migrated 20230207084556 in 0.0s

18:06:12.697 [info] == Running 20230207084558 PhoenixHelpdesk.Repo.Migrations.InitialMigration.up/0 forward

18:06:12.697 [info] create table tickets

18:06:12.703 [info] create table representatives

18:06:12.705 [info] alter table tickets

18:06:12.712 [info] alter table representatives

18:06:12.715 [info] == Migrated 20230207084558 in 0.0s
```

If your terminal looks like this then you have successfully created your ash postgres resources.
