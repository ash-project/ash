# Get Started

<!--- ash-hq-hide-start --> <!--- -->

This documentation is best viewed at [ash-hq.org](https://ash-hq.org)

<!--- ash-hq-hide-stop --> <!--- -->

## Goals

In this guide we will:

1. Create a new Phoenix project
2. Setup Ash, AshPhoenix and AshPostgres as dependencies
3. Create a very simple blog post resource
4. Create and migrate the database
5. Perform CRUD actions on newly made resource

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

### Create a New Phoenix Project

_This section is based the [Phoenix installation docs](https://hexdocs.pm/phoenix/installation.html). For more details go there._

We first need to create a fresh Phoenix project using the Phoenix project generator.

```bash
mix archive.install hex phx_new
```

And then run the project generator. **NOTE: DO NOT run `mix ecto.create`** we will do this the Ash way later.

```bash
mix phx.new my_ash_phoenix_app
```

This will then generate a project boilerplate and ask if you want to fetch and install dependencies.
**Answer 'Yes' to 'Fetch and install dependencies? [Yn]'**.

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

Please `cd` into the project directory, but again **DO NOT RUN `mix ecto.create`.**

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
      # If using ElixirLS then including elixir_sense to enable Ash auto-complete
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

Now in the terminal install these new dependencies.

```bash
mix deps.get
```

### Use `AshPostgres.Repo` and Create the Database

Swap `Ecto.Repo` for `AshPostgres.Repo`
Change your repo module to look like this:

```elixir
# lib/my_ash_phoenix_app/repo.ex

defmodule MyAshPhoenixApp.Repo do
  use AshPostgres.Repo, opt_app: :my_ash_phoenix_app

  # installs postgres extensions that ash commonly uses
  def installed_extensions do
    ["uuid-ossp", "citext"]
  end
end
```

`AshPostgres.Repo` enriches your repo with additional AshPostgres specific behaviour, but is essentially a thin wrapper around `Ecto.Repo`.

After swapping `AshPostgres.Repo` in you are safe to create the database using:

```
> mix ash_postgres.create

Compiling 17 files (.ex)
Generated my_ash_phoenix_app app
The database for MyAshPhoenixApp.Repo has been created
```

### Edit Config

We need to specify the Ash APIs that our application uses and some config for backwards compatibility that will be removed later.
Add this to your config:

```elixir
# config/config.exs

import Config

# For backwards compatibility, the following configuration is required.
# see https://ash-hq.org/docs/guides/ash/latest/get-started#temporary-config for more details
config :ash, :use_all_identities_in_manage_relationship?, false

config :my_ash_phoenix_app,
  ash_apis: [MyAshPhoenixApp.Blog]
```

### Create the API

You may have noticed that `MyAshPhoenixApp.Blog` doesn't actually exist yet. Lets make it now.

```elixir
# lib/my_ash_phoenix_app/blog.ex

defmodule MyAshPhoenixApp.Blog do
  use Ash.Api

  resources do
    registry MyAshPhoenixApp.Blog.Registry
  end
end
```

This tiny module tells Ash that `MyAshPhoenixApp.Blog` is an Ash API. It can be thought of as a [Bounded Context](https://martinfowler.com/bliki/BoundedContext.html) in Domain Driven Design terms and can seen as analogous to a Phoenix context.

It also tells where to find the resources within this API - in the `MyAshPhoenixApp.Blog.Registry` which we will make now.

### Create Registry

```elixir
# lib/my_ash_phoenix_app/blog/registry.ex

defmodule MyAshPhoenixApp.Blog.Registry do
  use Ash.Registry,
    extensions: [
      # This extension adds helpful compile time validations
      Ash.Registry.ResourceValidations
    ]

  entries do
    entry MyAshPhoenixApp.Blog.Post
  end
end
```

In the registry we enumerate the Ash resources we want our Ash API to contain. In this case its just one resource called `Post`.

### Creating the `Post` Resource

It's convention to place all the resource in their own resources folder. So when we create `Post` we will place it in `lib/my_ash_phoenix_project/blog/resources/post.ex`. So the structure after making the resource should look like so:

```
lib/
├─ my_ash_phoenix_app/
│  ├─ blog/
│  │  ├─ registry.ex
│  │  ├─ resources/
│  │  │  ├─ post.ex
│  ├─ blog.ex
```

Below is resource module. Read the comments carefully, every line is explained.

```elixir
defmodule MyAshPhoenixApp.Blog.Post do
  # Using Ash.Resource here turns this model into an Ash resource.
  use Ash.Resource,
    # Tells Ash you want this resource to store its data in postgres.
    data_layer: AshPostgres.DataLayer

  # The postgres keyword is specific to the AshPostgres module.
  # It tells postgres what to call the table and to
  # communicate with postgres through MyPhoenixApp.Repo
  postgres do
    # Tells postgres what to call the table
    table "posts"
    # Tells Ash how to interface with the postgres table
    repo MyAshPhoenixApp.Repo
  end

  actions do
    # Exposes default built in actions to modify the resource
    defaults [:create, :read, :update, :destroy]
  end

  # Attributes are the simple pieces of data that exist on your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id
    # Add a string type attribute called `:title`
    attribute :title, :string do
      # We don't want the title to ever be `nil`
      allow_nil? false
    end

    # Add a string type attribute called `:content`
    # If not specified content can be `nil`
    attribute :content, :string
  end
end
```

### Migrate Database

We have specified the resource attributes and actions in Ash. But we have yet to create them in our data layer (in our case postgres).

We created our database earlier, but now we need to populate it. We do this by generating and performing a migration.

We can use a generator to produce a migration for us. Ash can deduce what needs to go into the migration and do the hard work for us, just use the command below.

```bash
> mix ash_postgres.generate_migrations --name initial_migration

Extension Migrations:
* creating priv/resource_snapshots/extensions.json
* creating priv/repo/migrations/20230208045100_install_2_extensions.exs

Generating Tenant Migrations:

Generating Migrations:
* creating priv/repo/migrations/20230208045101_initial_migration.exs
```

The file we are concerned with now is `20230208045101_initial_migration.exs`.

```elixir
defmodule MyAshPhoenixApp.Repo.Migrations.InitialMigration do
  use Ecto.Migration

  # this function runs when migrating forward
  def up do
    # creates the `:post` table
    create table(:posts, primary_key: false) do
      # adds primary key attribute `:id` of type `:uuid`
      # nil values are not allowed
      add :id, :uuid, null: false, default: fragment("uuid_generate_v4()"), primary_key: true

      # adds attribute `:title` of type `:text`, nil values are not allowed
      add :title, :text, null: false
      # adds attribute `:content` of type `:text`, nil values are allowed
      add :content, :text
    end
  end

  # this is the function that runs if you want to rollback this migration.
  def down do
    # deletes the `:post` table
    drop table(:posts)
  end
end
```

We can run this function which will run this up function and perform these operations on the postgres database. We Run this command:

```bash
> mix ash_postgres.migrate

13:57:23.094 [info] == Running 20230208045100 MyAshPhoenixApp.Repo.Migrations.Install2Extensions.up/0 forward

13:57:23.101 [info] execute "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

13:57:23.131 [info] execute "CREATE EXTENSION IF NOT EXISTS \"citext\""

13:57:23.163 [info] == Migrated 20230208045100 in 0.0s

13:57:23.213 [info] == Running 20230208045101 MyAshPhoenixApp.Repo.Migrations.InitialMigration.up/0 forward

13:57:23.213 [info] create table posts

13:57:23.220 [info] == Migrated 20230208045101 in 0.0s
```

If your terminal looks like this then you have successfully created your first ash postgres resource.

### Interacting with your resource

All interaction with your resource attributes **ALWAYS** occur through an **action**. In our resource we are using the default actions for `:create, :read, :update, :destroy`. Create and update actions **ALWAYS** take a changeset. Ash changesets are conceptually similar to [Ecto changesets](https://hexdocs.pm/ecto/Ecto.Changeset.html). They're data structures which represent an intended change to an Ash resource.

Let's write a test to show how to interact with our resource.

```elixir
defmodule MyAshPhoenixApp.Blog.PostTest do
  use MyAshPhoenixApp.DataCase, async: true

  test "testing blog post actions" do
    ### CREATE ACTION - create new blog post ###
    # We create an Ash changeset we intent to use to create an Ash resource
    create_post_changeset =
      Ash.Changeset.for_create(
        # We specify which resource we want to create a changeset for
        MyAshPhoenixApp.Blog.Post,
        # We name the specific create action
        :create,
        # We pass the attributes we want to initialise our resource with
        %{title: "hello world"}
      )

    # will return:
    #
    # #Ash.Changeset<
    #   action_type: :create,
    #   action: :create,
    #   attributes: %{title: "hello world"},
    #   relationships: %{},
    #   errors: [],
    #   data: #MyAshPhoenixApp.Blog.Post<
    #     __meta__: #Ecto.Schema.Metadata<:built, "posts">,
    #     id: nil,
    #     title: nil,
    #     content: nil,
    #     aggregates: %{},
    #     calculations: %{},
    #     __order__: nil,
    #     ...
    #   >,
    #   valid?: true
    # >

    # This changeset is given to the Ash Api the resource belongs to (Blog in our case).
    # The Api then tries to create the resource specified in the changeset.
    assert %{title: "hello world"} = MyAshPhoenixApp.Blog.create!(create_post_changeset)
    # will return:
    #
    # #MyAshPhoenixApp.Blog.Post<
    #   __meta__: #Ecto.Schema.Metadata<:loaded, "posts">,
    #   id: "d70dd979-0b30-4a3f-beb2-2d5bb2e24af7",
    #   title: "hello world",
    #   content: nil,
    #   aggregates: %{},
    #   calculations: %{},
    #   __order__: nil,
    #   ...
    # >

    ...
```

Now let's read all of the data in the resource to check the creation we just performed worked. Notice how we don't need changeset here as we are not changing the data, we're just reading it.

```elixir
  ...

  ### READ ACTION - read blog post(s) ###
  # we need first_post for update action later
    assert [first_post = %{title: "hello world"}] =
             MyAshPhoenixApp.Blog.read!(MyAshPhoenixApp.Blog.Post)

    # will return:
    #
    # [
    #   #MyAshPhoenixApp.Blog.Post<
    #     __meta__: #Ecto.Schema.Metadata<:loaded, "posts">,
    #     id: "d70dd979-0b30-4a3f-beb2-2d5bb2e24af7",
    #     title: "hello world",
    #     content: nil,
    #     aggregates: %{},
    #     calculations: %{},
    #     __order__: nil,
    #     ...
    #   >
    # ]

    ...
```

Our post doesn't have any contents. Lets put some text in the `:content` attribute.
For this we will use an `:update` action:

```elixir
  ...

  ### UPDATE ACTION - update existing blog post ###
    # notice how you have to parse in an existing resource to the changeset
    assert %{
             title: "hello world",
             content: "hello to you too!"
           } =
             Ash.Changeset.for_update(first_post, :update, %{content: "hello to you too!"})
             |> MyAshPhoenixApp.Blog.update!()

    # will return:
    #
    # #MyAshPhoenixApp.Blog.Post<
    #   __meta__: #Ecto.Schema.Metadata<:loaded, "posts">,
    #   id: "d70dd979-0b30-4a3f-beb2-2d5bb2e24af7",
    #   title: "hello world",
    #   content: "hello to you too!",
    #   aggregates: %{},
    #   calculations: %{},
    #   __order__: nil,
    #   ...
    # >

  ...
```

Finally lets delete the post, and check its been removed from the resource, by performing one last read actions.

```elixir
  ...

  ### DELETE ACTION - delete existing blog post ###
    assert :ok ==
             Ash.Changeset.for_destroy(first_post, :destroy)
             |> MyAshPhoenixApp.Blog.destroy!()

    # verifying no rows in resource
    assert [] == MyAshPhoenixApp.Blog.read!(MyAshPhoenixApp.Blog.Post)
  end
end
```

Now lets run the test.

```bash
> mix test test/my_ash_phoenix_app/blog/resources/post_test.exs

.
Finished in 0.2 seconds (0.2s async, 0.00s sync)
1 test, 0 failures

Randomized with seed 333442
```

It works 🎉🥳!

### Where to Next?

We are just brushing the surface here, there is really so much more to Ash.
Here are a few things that we recommend looking at next.

#### Continue Learning

There's a few places you can go to learn more about how to use ash:

- [Read about how to query the data in your resources.](https://ash-hq.org/docs/module/ash/latest/ash-query)
- [Learn about how call your resources even more easily with the `code_interface`](https://ash-hq.org/docs/guides/ash/latest/topics/code-interface)
- [Dig deeper into actions.](https://ash-hq.org/docs/guides/ash/latest/topics/actions)
- [Study resource relationship management](https://ash-hq.org/docs/guides/ash/latest/topics/managing-relationships)

#### Ash Authentication & Ash Authentication Phoenix

See the power ash can bring to your web app or api. [Get authentication working in minutes](https://hexdocs.pm/ash_authentication_phoenix/getting-started-with-ash-authentication-phoenix.html).

#### Add an API (or two)

Check out the [AshJsonApi](https://ash-hq.org/docs/guides/ash_json_api/latest/tutorials/getting-started-with-json-api) and [AshGraphql](https://ash-hq.org/docs/guides/ash_graphql/latest/tutorials/getting-started-with-graphql) extensions to effortlessly build APIs around your resources.
