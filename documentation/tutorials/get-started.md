# Get Started

> ### Learn with Livebook {: .tip}
>
> We have a basic step by step tutorial in Livebook that introduces you to Ash. No prior Ash knowledge is required.
> The Livebook tutorial is self contained and separate from the documentation below.
> [![Run in Livebook](https://livebook.dev/badge/v1/pink.svg)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2Fash-project%2Fash_tutorial%2Fblob%2Fmaster%2Foverview.livemd)

## Goals

In this guide we will:

1. Create a new Elixir application and add Ash as a dependency
2. Create a simple set of resources and see how they can be used
3. Go over some core concepts of Ash
4. Find out what material might be good to visit next

## Requirements

If you want to follow along yourself, you will need the following things:

1. [Elixir and Erlang installed](https://elixir-lang.org/install.html)
2. A text editor to make the changes that we make
3. A terminal to run the examples using `iex`

## Steps

For this tutorial, we'll use examples based around creating a help desk.

We will make the following resources:

- `Helpdesk.Support.Ticket`
- `Helpdesk.Support.Representative`

The actions we will be able to take on these resources include:

- Opening a new Ticket
- Closing a Ticket
- Assigning a Ticket to a representative

### Create a new project

<!-- tabs-open -->

### Using Igniter (recommended)

First, to use `mix igniter.new`, the archive must be installed.

To install it, run

```bash
mix archive.install hex igniter_new
```

Then, create a new project:

```elixir
mix igniter.new helpdesk --install ash && cd helpdesk
```

If you already know that you want to use Phoenix and Ash together, you can use

```elixir
# install the archive
mix archive.install hex phx_new

# use the `--with` flag to generate the project with phx.new and add Ash
mix igniter.new helpdesk --install ash --with phx.new && cd helpdesk
```

It is a good idea to make it a git repository and commit the initial project. You'll be able to see what changes we made, and can save your changes once we're done.

```bash
# Run in your terminal
git init
git add -A
git commit -m "first commit"
git branch -M main
```

Open the project in your text editor, and we'll get started.

> ### Want to skip to the end? {: .info}
>
> Add the `--example` flag to get the example code add directly to your app!
>
> ```bash
> mix igniter.new helpdesk --install ash --extend ets --example
> ```
>
> Already know you want to use `AshPostgres`? Use the `--extend` argument.
>
> ```bash
> mix igniter.new helpdesk --install ash,ash_postgres --example --extend postgres`
> ```
>
> Want to start with a Phoenix app setup too? Use the `--with` argument.
>
> ```bash
> mix archive.install hex phx_new
>
> mix igniter.new helpdesk \
>   --install ash,ash_postgres \
>   --with phx.new \
>   --extend postgres \
>   --example
> ```
>
> If you generate this code, you can browse the rest of the guide, but the code shown will already be present in your application 🥳

### Using Mix

We first create a new project with the `--sup` flag to add a supervision tree. This will be necessary for other follow-up tutorials.

```bash
# In your terminal
mix new --sup helpdesk && cd helpdesk
```

It is a good idea to make it a git repository and commit the initial project. You'll be able to see what changes we made, and can save your changes once we're done.

```bash
# Run in your terminal
git init
git add -A
git commit -m "first commit"
git branch -M main
```

Open the project in your text editor, and we'll get started.

### Add Ash to your application

Add the `ash` and `picosat_elixir` dependencies to your `mix.exs`

```elixir
defp deps do
  [
    {:ash, "~> 3.0"},
    {:picosat_elixir, "~> 0.2"}
  ]
end
```

And then run `mix deps.get && mix deps.compile` to install the dependencies

#### Formatting

To ensure that your code stays formatted like the examples here, you can add `:ash` as an import dependency in your `.formatter.exs`:

```elixir
[
  # ...
  import_deps: [..., :ash],
  # ...
]
```

<!-- tabs-close -->

> ### Picosat installation issues? {: .info}
>
> If you have trouble compiling `picosat_elixir`, then replace `{:picosat_elixir, "~> 0.2"}` with `{:simple_sat, "~> 0.1"}` to use a simpler (but mildly slower) solver. You can always switch back to `picosat_elixir` later once you're done with the tutorial.

> #### Note {: .neutral}
>
> For more auto-formatting options, see the [Development Utilities guide](/documentation/topics/development/development-utilities.md).

And run `mix deps.get`, to install the dependency.

### Building your first Ash Domain

The basic building blocks of an Ash application are Ash resources. They are tied together by a domain module, which will allow you to interact with those resources.

### Creating our first resource

Let's start by creating our first resource along with our first domain. We will create the following files:

- The domain `Helpdesk.Support`, in `lib/helpdesk/support.ex`
- Our Ticket resource `Helpdesk.Support.Ticket`, in `lib/helpdesk/support/ticket.ex`.

To create the required folders and files, you can use the following command in your terminal:

```bash
mkdir -p lib/helpdesk/support && touch $_/ticket.ex
touch lib/helpdesk/support.ex
```

Your project structure should now include the following files:

```
lib/
├─ helpdesk/
│  ├─ support/
│  │  ├─ ticket.ex
│  ├─ support.ex
```

Add the following to the files we created

```elixir
# lib/helpdesk/support.ex

defmodule Helpdesk.Support do
  use Ash.Domain

  resources do
    resource Helpdesk.Support.Ticket
  end
end
```

```elixir
# lib/helpdesk/support/ticket.ex

defmodule Helpdesk.Support.Ticket do
  # This turns this module into a resource
  use Ash.Resource, domain: Helpdesk.Support

  actions do
    # Use the default implementation of the :read action
    defaults [:read]

    # and a create action, which we'll customize later
    create :create
  end

  # Attributes are the simple pieces of data that exist on your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id

    # Add a string type attribute called `:subject`
    attribute :subject, :string
  end
end
```

Next, add your domain to your `config.exs`, and configure some backwards compatibility configuration.

Run the following to create your `config.exs` if it doesn't already exist

```elixir
mkdir -p config
touch config/config.exs
```

and add the following contents to it.

```elixir
# in config/config.exs
import Config

config :helpdesk, :ash_domains, [Helpdesk.Support]

config :ash,
  include_embedded_source_by_default?: false,
  default_page_type: :keyset

config :ash, :policies,
  no_filter_static_forbidden_reads?: false
```

### Try our first resource out

Run `iex -S mix` in your project's root directory and try out the following.

To create a ticket, we first make an `Ash.Changeset` for the `:create` action of the `Helpdesk.Support.Ticket` resource. Then we pass it to the `Ash.create!/1` function.

```elixir
Helpdesk.Support.Ticket
|> Ash.Changeset.for_create(:create)
|> Ash.create!()
```

This returns what we call a `record` which is an instance of a resource.

```elixir
#Helpdesk.Support.Ticket<
  ...,
  id: "c0f8dc32-a018-4eb4-8656-d5810118f4ea",
  subject: nil,
  ...
>
```

### Customizing our Actions

One thing you may have noticed earlier is that we created a ticket without providing any input, and as a result our ticket had a `subject` of `nil`. Additionally, we don't have any other data on the ticket. Lets add a `status` attribute, ensure that `subject` can't be `nil`, and provide a better interface by giving the `:create` action a better name, and accepting `:subject` as part of the action.

We'll start with the attribute changes:

```elixir
# lib/helpdesk/support/ticket.ex

attributes do
  ...
  attribute :subject, :string do
    # Don't allow `nil` values
    allow_nil? false

    # Allow this attribute to be public. By default, all attributes are private.
    public? true
  end

  # status is either `open` or `closed`. We can add more statuses later
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
```

And then replace the `:create` action with `:open`, and accept `:subject` as input.

```elixir
# lib/helpdesk/support/ticket.ex

actions do
  ...
  create :open do
    accept [:subject]
  end
end
```

Let's try these changes in `iex`:

We use `create!` with an exclamation point here because that will raise the error which gives a nicer view of the error in `iex`

```elixir
# Use this to pick up changes you've made to your code, or restart your session
recompile()

Helpdesk.Support.Ticket
|> Ash.Changeset.for_create(:open, %{subject: "My mouse won't click!"})
|> Ash.create!()
```

And we can see our newly created ticket with a subject and a status.

```elixir
#Helpdesk.Support.Ticket<
  ...
  id: "3c94d310-7b5e-41f0-9104-5b193b831a5d",
  status: :open,
  subject: "My mouse won't click!",
  ...
>
```

If we didn't include a subject, or left off the arguments completely, we would see an error instead

```text
** (Ash.Error.Invalid) Invalid Error

* attribute subject is required
```

### Updates and validations

Now let's add some logic to close a ticket. This time we'll add an `update` action.

Here we will use a `change`. Changes allow you to customize how an action executes with very fine-grained control. There are built-in changes that are automatically available as functions, but you can define your own and pass it in as shown below. You can add multiple, and they will be run in order. See the `Ash.Changeset` module documentation for more.

```elixir
# lib/helpdesk/support/ticket.ex

actions do
  ...
  update :close do
    # We don't want to accept any input here
    accept []

    validate attribute_does_not_equal(:status, :closed) do
      message "Ticket is already closed"
    end

    change set_attribute(:status, :closed)
    # A custom change could be added like so:
    #
    # change MyCustomChange
    # change {MyCustomChange, opt: :val}
  end
end
```

Try out opening and closing a ticket in `iex`:

```elixir
# Use this to pick up changes you've made to your code, or restart your session
recompile()

# parenthesis so you can paste into iex
ticket = (
  Helpdesk.Support.Ticket
  |> Ash.Changeset.for_create(:open, %{subject: "My mouse won't click!"})
  |> Ash.create!()
)

ticket
|> Ash.Changeset.for_update(:close)
|> Ash.update!()

#Helpdesk.Support.Ticket<
  ...
  status: :closed,
  subject: "My mouse won't click!",
  ...
>
```

### Querying without persistence

So far we haven't used a data layer that does any persistence, like storing records in a database. All that this simple resource does is return the record back to us. You can see this lack of persistence by attempting to use a `read` action:

```elixir
Ash.read!(Helpdesk.Support.Ticket)
```

Which will raise an error explaining that there is no data to be read for that resource.

In order to save our data somewhere, we need to add a data layer to our resources. Before we do that, however, let's go over how Ash allows us to work against many different data layers (or even no data layer at all).

Resources without a data layer will implicitly be using `Ash.DataLayer.Simple`. This data is not persisted anywhere, and must be provided when running queries. It provides a utility for just this purpose, `Ash.DataLayer.Simple.set_data/2`.

Try the following in `iex`. We will open some tickets, and close some of them, and then use `Ash.DataLayer.Simple.set_data/2` to use those tickets.

```elixir
# Ash.Query is a macro, so it must be required
require Ash.Query

tickets =
  for i <- 0..5 do
    ticket =
      Helpdesk.Support.Ticket
      |> Ash.Changeset.for_create(:open, %{subject: "Issue #{i}"})
      |> Ash.create!()

    if rem(i, 2) == 0 do
      ticket
      |> Ash.Changeset.for_update(:close)
      |> Ash.update!()
    else
      ticket
    end
  end
```

Find the tickets where the subject contains `"2"`. Note that the we're setting the ticket data that we're querying using `set_data`.

```elixir
Helpdesk.Support.Ticket
|> Ash.Query.filter(contains(subject, "2"))
|> Ash.DataLayer.Simple.set_data(tickets)
|> Ash.read!()
```

Find the tickets that are _closed_ and their subject does _not_ contain `"4"`

```elixir
Helpdesk.Support.Ticket
|> Ash.Query.filter(status == :closed and not(contains(subject, "4")))
|> Ash.DataLayer.Simple.set_data(tickets)
|> Ash.read!()
```

The examples above could be easily implemented with `Enum.filter`, but the real power here is to allow you to use the same tools when working with any data layer.

Even though it doesn't persist data in any way, `Ash.DataLayer.Simple` can be useful to model static data, or be used for resources where all the actions are manual and inject data from other sources.

### Adding basic persistence

Before we get into working with relationships, let's add some real persistence to our resource. This will let us add relationships and try out querying data.

There is a built in data layer that is useful for testing and prototyping, that uses [ETS](https://elixir-lang.org/getting-started/mix-otp/ets.html). ETS (Erlang Term Storage) is OTP's in-memory database, so the data won't actually stick around beyond the lifespan of your program, but it's a simple way to try things out.

To add it to your resource, modify it like so:

```elixir
# lib/helpdesk/support/ticket.ex

use Ash.Resource,
  domain: Helpdesk.Support,
  data_layer: Ash.DataLayer.Ets
```

Now we can slightly modify our code above, by removing the `Ash.DataLayer.Simple.set_data/2` calls, and we can see our persistence in action. Remember, ETS is in-memory, meaning restarting your application/iex session will remove all of the data.

```elixir
# Use this to pick up changes you've made to your code, or restart your session
recompile()

require Ash.Query

for i <- 0..5 do
  ticket =
    Helpdesk.Support.Ticket
    |> Ash.Changeset.for_create(:open, %{subject: "Issue #{i}"})
    |> Ash.create!()

  if rem(i, 2) == 0 do
    ticket
    |> Ash.Changeset.for_update(:close)
    |> Ash.update!()
  end
end

# Show the tickets where the subject contains "2"
Helpdesk.Support.Ticket
|> Ash.Query.filter(contains(subject, "2"))
|> Ash.read!()

# Show the tickets that are closed and their subject does not contain "4"
Helpdesk.Support.Ticket
|> Ash.Query.filter(status == :closed and not(contains(subject, "4")))
|> Ash.read!()
```

### Adding relationships

Now we want to be able to assign a Ticket to a Representative. First, let's create the Representative resource:

```elixir
# lib/helpdesk/support/representative.ex

defmodule Helpdesk.Support.Representative do
  # This turns this module into a resource using the in memory ETS data layer
  use Ash.Resource,
    domain: Helpdesk.Support,
    data_layer: Ash.DataLayer.Ets

  actions do
    # Add the default simple actions
    defaults [:read]

    create :create do
      accept [:name]
    end
  end

  # Attributes are the simple pieces of data that exist on your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id

    # Add a string type attribute called `:name`
    attribute :name, :string do
      # Make the attribute public in order to give a name when calling functions from `Ash.Changeset`.
      public? true
    end
  end

  relationships do
    # `has_many` means that the destination attribute is not unique, therefore many related records could exist.
    # We assume that the destination attribute is `representative_id` based
    # on the module name of this resource and that the source attribute is `id`.
    has_many :tickets, Helpdesk.Support.Ticket
  end
end
```

Now let's modify our Ticket resource to have the inverse relationship to the Representative.

```elixir
# lib/helpdesk/support/ticket.ex

relationships do
  # belongs_to means that the destination attribute is unique, meaning only one related record could exist.
  # We assume that the destination attribute is `representative_id` based
  # on the name of this relationship and that the source attribute is `representative_id`.
  # We create `representative_id` automatically.
  belongs_to :representative, Helpdesk.Support.Representative
end
```

Finally, let's add our new Representative resource to our domain module

```elixir
# lib/helpdesk/support.ex

resources do
 ...
 resource Helpdesk.Support.Representative
end
```

You may notice that if you don't add the resource to your domain, or if you don't add the `belongs_to` relationship, that you'll get helpful errors at compile time. Helpful compile time validations are a core concept of Ash as we really want to ensure that your application is valid.

## Working with relationships

The simplest way to work with belongs to relationships is to allow directly editing the underlying id field.

> ### managing relationships {: .tip}
>
> There are a wide array of options when managing relationships, and we won't cover all of them here. See the [Managing Relationships guide](/documentation/topics/resources/relationships.md#managing-relationships) for more.

Add the `assign` action to allow us to assign a Ticket to a Representative.

```elixir
# lib/helpdesk/support/ticket.ex

update :assign do
  accept [:representative_id]
end
```

Let's try it out in our `iex` console!

Use `recompile` to pick up changes you've made to your code, or just restart your session.

```elixir
recompile()
```

### Open a Ticket

```elixir
ticket = (
  Helpdesk.Support.Ticket
  |> Ash.Changeset.for_create(:open, %{subject: "I can't find my hand!"})
  |> Ash.create!()
)
```

### Create a Representative

```elixir
representative = (
  Helpdesk.Support.Representative
  |> Ash.Changeset.for_create(:create, %{name: "Joe Armstrong"})
  |> Ash.create!()
)
```

### Assign that Representative to the Ticket

```elixir
ticket
|> Ash.Changeset.for_update(:assign, %{representative_id: representative.id})
|> Ash.update!()
```

### What next?

What you've seen above barely scratches the surface of what Ash can do. In a lot of ways, it will look very similar to other tools that you've seen. If all that you ever used was the above, then realistically you won't see much benefit to using Ash.

Where Ash shines however, is all of the tools that can work _with_ your resources. You have the ability to extend the framework yourself, and apply consistent design patterns that enable unparalleled efficiency, power and flexibility as your application grows.

#### Get Help

- Check out [ElixirForum](https://elixirforum.com/c/ash-framework-forum)
- Join our [Discord server](https://discord.gg/D7FNG2q)
- Open a [GitHub issue](https://github.com/ash-project/ash/issues/new/choose)

#### Persist your data

See [The AshPostgres getting started guide](https://hexdocs.pm/ash_postgres) to see how to back your resources with Postgres. This is highly recommended, as the Postgres data layer provides tons of advanced capabilities.

#### Add a web API

Check out [AshJsonApi](https://hexdocs.pm/ash_json_api) and [AshGraphql](https://hexdocs.pm/ash_graphql) extensions to build APIs around your resource

#### Authorize access and work with users

See the [Policies guide](/documentation/topics/security/policies.md) for information on how to authorize access to your resources using actors and policies.

#### Clean up your code that uses Ash?

Creating and using changesets manually can be verbose, and they all look very similar. Luckily, Ash has your back and can help you build high quality interfaces for you!

Check out the [Code Interface Guide](/documentation/topics/resources/code-interfaces.md) to derive things like `Helpdesk.Support.Ticket.assign!(representative.id)`
