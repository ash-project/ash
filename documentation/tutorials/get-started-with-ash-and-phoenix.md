# Get Started with Ash and Phoenix

<!--- ash-hq-hide-start --> <!--- -->

This documentation is best viewed at [ash-hq.org](https://ash-hq.org)

<!--- ash-hq-hide-stop --> <!--- -->

## Goals

In this guide we will:

1. Create a new Phoenix project
2. Setup Ash, AshPhoenix and AshPostgres as dependencies
3. Create a very simple `Blog.Post` resource
4. Create and migrate the database
5. Perform CRUD actions on the newly made resource

## Things you may want to read first

- [Install Elixir](https://elixir-lang.org/install.html)
- [Phoenix - Up and Running Guide](https://hexdocs.pm/phoenix/up_and_running.html)
- [Philosophy Guide](/documentation/tutorials/philosophy.md)

## Requirements

If you want to follow along yourself, you will need the following things:

1. Elixir (1.12 or later) and Erlang (22 or later) installed
2. PostgreSQL installed
3. A text editor
4. A terminal to run the examples

## Setup

### Create a New Phoenix Project

_This section is based the [Phoenix installation docs](https://hexdocs.pm/phoenix/installation.html). For more details go there._

We first need to create a fresh Phoenix project using the Phoenix project generator.
First we need to install the Phoenix project generator, then we'll run the generator to create our new project.

**NOTE: DO NOT run `mix ecto.create`, (as it asks you to) we will do this the Ash way later.**

```bash
# install Phoenix project generator
$ mix archive.install hex phx_new

# generate Phoenix project
$ mix phx.new my_ash_phoenix_app

# cd into project
$ cd my_ash_phoenix_app
```

### Add Dependencies

We now need to add Ash, AshPhoenix and AshPostgres to our Phoenix project. We need to add the dependencies to the `deps` function in our `mix.exs`. We'll also need to add dependencies to our `.formatter.exs` to ensure consistent formatting when using `mix format`.

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
$ mix deps.get
```

### Use `AshPostgres.Repo` and Create the Database

We need to swap `Ecto.Repo` for `AshPostgres.Repo`. `AshPostgres.Repo` enriches your repo with additional AshPostgres specific behaviour, but is essentially a thin wrapper around `Ecto.Repo`.
To use `AshPostgres.Repo` change your repo module to look like this:

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

After swapping `AshPostgres.Repo` in you are safe to create the database using:

```
$ mix ash_postgres.create

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

### Create the API and Registry

An Ash API can be thought of as a [Bounded Context](https://martinfowler.com/bliki/BoundedContext.html) in Domain Driven Design terms and can seen as analogous to a Phoenix context. If none of that made sense, don't worry most of the time you only need one.

**Ash API's point to one or more Ash Registries** each of which can be configured with different extensions. In our case we have one API, `MyAshPhoenixApp.Blog`.

**Each registry contains entries of one or more resources.** In our case we have one resource `MyAshPhoenixApp.Blog.Post`.
Below is the `Blog` api and the `Blog.Registry` registry:

```elixir
# lib/my_ash_phoenix_app/blog.ex

defmodule MyAshPhoenixApp.Blog do
  use Ash.Api

  resources do
    registry MyAshPhoenixApp.Blog.Registry
  end
end
```

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

## Creating Resources

### Creating the `Post` Resource

We only have one thing left to create, the resource.

A resource is a central concept in Ash. In short, a resource is a domain model object in your system. A resource defines the data it holds and defines the actions that can operate on that data.

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

Below is resource module. Read the comments carefully, every line is explained:

```elixir
defmodule MyAshPhoenixApp.Blog.Post do
  # Using Ash.Resource turns this module into an Ash resource.
  use Ash.Resource,
    # Tells Ash you want this resource to store its data in postgres.
    data_layer: AshPostgres.DataLayer

  # The postgres keyword is specific to the AshPostgres module.
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

  # Attributes are simple pieces of data that exist in your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id
    # Add a string type attribute called `:title`
    attribute :title, :string do
      # We don't want the title to ever be `nil`
      allow_nil? false
    end

    # Add a string type attribute called `:content`
    # If not `allow_nil?` specified content can be `nil`
    attribute :content, :string
  end
end
```

### Migrate Database

We have specified the resource, attributes and actions in Ash. But we have yet to create them in our data layer (in our case postgres).

We created our database earlier, but now we need to populate it. We do this by generating and performing a migration.

We can use a generator to produce a migration for us. Ash can deduce what needs to go into the migration and do the hard work for us, just use the command below:

```bash
$ mix ash_postgres.generate_migrations --name initial_migration

# ... don't worry about other files it creates

Generating Migrations:
* creating priv/repo/migrations/20230208045101_initial_migration.exs
```

Inside the migration file we find:

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

We can run the `up/0` function which will perform the desired operations on the postgres database. We do this with this command:

```bash
$ mix ash_postgres.migrate
```

## Interacting with your resource

All interaction with your resource attributes **ALWAYS** occur through an **action**. In our resource we are using the default actions for `:create, :read, :update, :destroy`. Create, update and destroy actions **ALWAYS** take a changeset. Ash changesets are conceptually similar to [Ecto changesets](https://hexdocs.pm/ecto/Ecto.Changeset.html). They're data structures which represent an intended change to an Ash resource and provide validation.

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

## But how to integrate with Phoenix?

Its simple, you can call the code in the tests above in your controller or LiveView.
Here are some simple examples.

In a controller:

```elixir
  def index(conn, _params) do
    conn
    |> assign(:posts, MyAshPhoenixApp.Blog.read!(MyAshPhoenixApp.Blog.Post))
    |> render("index.html")
  end
```

In a LiveView:

```elixir
  def mount(_params, _session, socket) do
    socket =
      assigns(socket, :posts, MyAshPhoenixApp.Blog.read!(MyAshPhoenixApp.Blog.Post))

    {:ok, socket}
  end
```

There are more idiomatic ways to interact with ash in the view layer, and we'll cover them. But this will do for now.

## Where to Next?

We are just brushing the surface here, there is really so much more to Ash.
Here are a few things that we recommend looking at next.

### Continue Learning

There's a few places you can go to learn more about how to use ash:

- [Learn about how call your resources even more easily with the `code_interface`](/documentation/topics/code-interface)
- [Read about how to query the data in your resources.](/documentation/module/ash/ash-query)
- [Dig deeper into actions.](/documentation/topics/actions)
- [Study resource relationship management](/documentation/topics/managing-relationships)

### Ash Authentication & Ash Authentication Phoenix

See the power ash can bring to your web app or api. [Get authentication working in minutes](https://hexdocs.pm/ash_authentication_phoenix/getting-started-with-ash-authentication-phoenix.html).

### Add an API (or two)

Check out the [AshJsonApi](/documentation/guides/ash_json_api/tutorials/getting-started-with-json-api) and [AshGraphql](/documentation/guides/ash_graphql/tutorials/getting-started-with-graphql) extensions to effortlessly build APIs around your resources.
