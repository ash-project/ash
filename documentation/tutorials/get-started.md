# Get Started

<!--- ash-hq-hide-start --> <!--- -->

> #### HexDocs {: .tip}
>
> Hexdocs does not support multi-package search. To assist with this, we provide a mirror of this documentation at [ash-hq.org](https://ash-hq.org). Use Ctrl+K or Cmd+K to search all packages on that site. For the best way to use the hex documentation, see the [hexdocs guide](/documentation/tutorials/using-hexdocs.md).

<!--- ash-hq-hide-stop --> <!--- -->

## Learn with Livebook

We have a basic step by step tutorial in Livebook that introduces you to Ash. No prior Ash knowledge is required.
The Livebook tutorial is self contained and separate from the documentation below.

[![Run in Livebook](https://livebook.dev/badge/v1/pink.svg)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2Fash-project%2Fash_tutorial%2Fblob%2Fmaster%2Foverview.livemd)

## Watch the ElixirConf Talk

<iframe width="560" height="315" class="rounded-xl w-full aspect-video" src="https://www.youtube.com/embed/c4iou77kOFc?si=gxPdzGng5cQTrr7P" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen />

## Goals

In this guide we will:

1. Create a new Elixir application and add Ash as a dependency
2. Create a simple set of resources and show they can be used
3. Illustrate some core concepts of Ash
4. Point you to good next resources so you can explore Ash further

## Things you may want to read first

- [Install Elixir](https://elixir-lang.org/install.html)
- [Philosophy Guide](/documentation/tutorials/philosophy.md)
- [Using Hexdocs](/documentation/tutorials/using-hexdocs.md)

## Requirements

If you want to follow along yourself, you will need the following things:

1. Elixir and Erlang installed
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

We first create a new project with the `--sup` flag to add a supervision tree. This will be necessary for later steps.

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

Add the `ash` dependency to your `mix.exs`

```elixir
defp deps do
  [
    # {:dep_from_hexpm, "~> 0.3.0"},
    # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
    {:ash, "~> 3.0"}, # <-- add this line
    {:picosat_elixir, "~> 0.2"} # <- and this line
  ]
end
```

### Picosat Installation Issues

In rare cases, users have trouble installing picosat (usually on windows)
if that is the case, use `simple_sat` instead. We _highly recommend_ that you
get `picosat_elixir` working before shipping to production if you intend to use
Ash policies. We've provided `simple_sat` to get you up and running more easily
and to allow you to explore Ash without roadblocks.

```elixir
defp deps do
  [
    # {:picosat_elixir, "~> 0.2"} # instead of this
    {:simple_sat, "~> 0.1"} # <- use this
  ]
end
```

### Formatting

To ensure that your code stays formatted like the examples here, you can add `:ash` as an import dependency in your `.formatter.exs`:

```elixir
[
  # ...
  import_deps: [..., :ash],
  # ...
]
```

> #### Note {: .neutral}
>
> For more auto-formatting options, see the [Auto-Format Code guide](/documentation/how_to/auto-format-code.md).

And run `mix deps.get`, to install the dependency.

### Building your first Ash API

The basic building blocks of an Ash application are Ash resources. They are tied together by an API module (not to be confused with a web API), which will allow you to interact with those resources.

It might be helpful to think of an Ash API as a Bounded Context (in the Domain Driven Design sense), or as a Service (in the microservice sense).

### Creating our first resource

Let's start by creating our first resource along with our first API. We will create the following files:

- The API [Helpdesk.Support] - `lib/helpdesk/support.ex`
- Our Ticket resource [Helpdesk.Support.Ticket] - `lib/helpdesk/support/ticket.ex`.

To create the required folders and files, you can use the following command in your terminal:

```bash
mkdir -p lib/helpdesk/support && touch $_/ticket.ex
touch lib/helpdesk/support.ex
```

Your project structure should now look like this:

```
lib/
├─ helpdesk/
│  ├─ support/
│  │  ├─ ticket.ex
│  ├─ support.ex
```

Add the following to the files we created

```elixir
# lib/helpdesk/support/ticket.ex

defmodule Helpdesk.Support.Ticket do
  # This turns this module into a resource
  use Ash.Resource

  actions do
    # Add a set of simple actions. You'll customize these later.
    defaults [:create, :read, :update, :destroy]
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

```elixir
# lib/helpdesk/support.ex

defmodule Helpdesk.Support do
  use Ash.Api

  resources do
    resource Helpdesk.Support.Ticket
  end
end
```

Next, add your api to your `config.exs`

Run the following to create your `config.exs` if it doesn't already exist

```elixir
mkdir -p config
touch config/config.exs
```

and add the following contents to it (if the file already exists, just make sure the `config` line is added)

```elixir
# in config/config.exs
import Config

config :helpdesk, :ash_apis, [Helpdesk.Support]
```

### Try our first resource out

Run `iex -S mix` in your project and try it out.

To create a ticket, we first make an `Ash.Changeset` for the `:create` action of the `Helpdesk.Support.Ticket` resource. Then we pass it to the `create!/1` function on our API module `Helpdesk.Support`.

```elixir
Helpdesk.Support.Ticket
|> Ash.Changeset.for_create(:create)
|> Helpdesk.Support.create!()
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

One thing you may have noticed earlier is that we created a ticket without providing any input, and as a result our ticket had a `subject` of `nil`. Additionally, we don't have any other data on the ticket. Lets add a `status` attribute, ensure that `subject` can't be `nil`, and provide a better interface by making a custom action for opening a ticket, called `:open`.

We'll start with the attribute changes:

```elixir
# lib/helpdesk/support/ticket.ex

attributes do
  ...
  attribute :subject, :string do
    # Don't allow `nil` values
    allow_nil? false
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

And then add our customized `open` action which should take a `subject` argument:

```elixir
# lib/helpdesk/support/ticket.ex

actions do
  ...
  create :open do
    # By default you can provide all public attributes to an action
    # This action should only accept the subject
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
|> Helpdesk.Support.create!()
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
** (Ash.Error.Invalid) Input Invalid

* attribute subject is required
```

### Updates and validations

Now let's add some logic to close a ticket. This time we'll add an `update` action.

Here we will use a `change`. Changes allow you to customize how an action executes with very fine-grained control. There are built-in changes that are automatically available as functions, but you can define your own and pass it in as shown below. You can add multiple, and they will be run in order. See the [Actions guide](/documentation/topics/actions.md) for more.

```elixir
# lib/helpdesk/support/ticket.ex

actions do
  ...
  update :close do
    # We don't want to accept any input here
    accept []

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
  |> Helpdesk.Support.create!()
)

ticket
|> Ash.Changeset.for_update(:close)
|> Helpdesk.Support.update!()

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
Helpdesk.Support.read!(Helpdesk.Support.Ticket)
```

Which will raise an error explaining that there is no data to be read for that resource.

In order to save our data somewhere, we need to add a data layer to our resources. Before we do that, however, let's go over how Ash allows us to work against many different data layers (or even no data layer at all).

Resources without a data layer will implicitly be using `Ash.DataLayer.Simple`, which will just return structs and won't actually store anything. The way that we make our queries return some data is by leveraging `context`, a free-form map available on queries and changesets. The simple data layer looks for `query.context[:data_layer][:data][resource]`. It provides a utility, `Ash.DataLayer.Simple.set_data/2` to set it.

Try the following in `iex`. We will open some tickets, and close some of them, and then use `Ash.DataLayer.Simple.set_data/2` to use those tickets.

```elixir
# Ash.Query is a macro, so it must be required
require Ash.Query

tickets =
  for i <- 0..5 do
    ticket =
      Helpdesk.Support.Ticket
      |> Ash.Changeset.for_create(:open, %{subject: "Issue #{i}"})
      |> Helpdesk.Support.create!()

    if rem(i, 2) == 0 do
      ticket
      |> Ash.Changeset.for_update(:close)
      |> Helpdesk.Support.update!()
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
|> Helpdesk.Support.read!()
```

Find the tickets that are _closed_ and their subject does _not_ contain `"4"`

```elixir
Helpdesk.Support.Ticket
|> Ash.Query.filter(status == :closed and not(contains(subject, "4")))
|> Ash.DataLayer.Simple.set_data(tickets)
|> Helpdesk.Support.read!()
```

The examples above could be easily implemented with `Enum.filter`, but the real power here is to allow you to use the same tools when working with any data layer. If you were using the `AshPostgres.DataLayer` data layer.

Even though it doesn't persist data in any way, `Ash.DataLayer.Simple` can be useful to model static data, or be used for resources where all the actions are manual and inject data from other sources.

### Adding basic persistence

Before we get into working with relationships, let's add some real persistence to our resource. This will let us add relationships and try out querying data.

There is a built in data layer that is useful for testing and prototyping, that uses [ETS](https://elixir-lang.org/getting-started/mix-otp/ets.html). ETS (Erlang Term Storage) is OTP's in-memory database, so the data won't actually stick around beyond the lifespan of your program, but it's a simple way to try things out.

To add it to your resource, modify it like so:

```elixir
# lib/helpdesk/support/ticket.ex

use Ash.Resource,
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
    |> Helpdesk.Support.create!()

  if rem(i, 2) == 0 do
    ticket
    |> Ash.Changeset.for_update(:close)
    |> Helpdesk.Support.update!()
  end
end

# Show the tickets where the subject contains "2"
Helpdesk.Support.Ticket
|> Ash.Query.filter(contains(subject, "2"))
|> Helpdesk.Support.read!()

# Show the tickets that are closed and their subject does not contain "4"
Helpdesk.Support.Ticket
|> Ash.Query.filter(status == :closed and not(contains(subject, "4")))
|> Helpdesk.Support.read!()
```

### Adding relationships

Now we want to be able to assign a Ticket to a Representative. First, let's create the Representative resource:

```elixir
# lib/helpdesk/support/representative.ex

defmodule Helpdesk.Support.Representative do
  # This turns this module into a resource using the in memory ETS data layer
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets

  actions do
    # Add the default simple actions
    defaults [:create, :read, :update, :destroy]
  end

  # Attributes are the simple pieces of data that exist on your resource
  attributes do
    # Add an autogenerated UUID primary key called `:id`.
    uuid_primary_key :id

    # Add a string type attribute called `:name`
    attribute :name, :string
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

Finally, let's add our new Representative resource to our Api module

```elixir
# lib/helpdesk/support.ex

resources do
 ...
 resource Helpdesk.Support.Representative
end
```

You may notice that if you don't add the resource to your api, or if you don't add the `belongs_to` relationship, that you'll get helpful errors at compile time. Helpful compile time validations are a core concept of Ash as we really want to ensure that your application is valid.

## Working with relationships

There are a wide array of options when managing relationships, and we won't cover all of them here. See the guide on [Managing Relationships](/documentation/topics/managing-relationships.md) for a full explanation.

In this example we'll demonstrate the use of action arguments, the method by which you can accept additional input to an action.

Add the `assign` action to allow us to assign a Ticket to a Representative.

```elixir
# lib/helpdesk/support/ticket.ex

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
  |> Helpdesk.Support.create!()
)
```

### Create a Representative

```elixir
representative = (
  Helpdesk.Support.Representative
  |> Ash.Changeset.for_create(:create, %{name: "Joe Armstrong"})
  |> Helpdesk.Support.create!()
)
```

### Assign that Representative to the Ticket

```elixir
ticket
|> Ash.Changeset.for_update(:assign, %{representative_id: representative.id})
|> Helpdesk.Support.update!()
```

### Assigning the Representative to a Ticket during its creation

With the current definition of the Ticket resource the following will execute without error, but the `representative_id` field of the newly generated ticket will still remain empty:

```elixir
Helpdesk.Support.Ticket
|> Ash.Changeset.for_create(:open, %{subject: "My spoon is too big!", representative_id: representative.id})
|> Helpdesk.Support.create!()
```

The reason is that `belongs_to` relationships are not marked as public and writable by default (refer to the [define_attribute?](https://ash-hq.org/docs/dsl/ash/latest/resource/relationships/belongs_to#attribute_writable?) option of `belongs_to`).

With the following modification the attribute can be written to, during the `:create` action:

```elixir
# lib/helpdesk/support/ticket.ex
...
actions do
  create :open do
    accept([:subject, :representative_id])
  end
end
...
relationships do
  belongs_to :representative, Helpdesk.Support.Representative do
    attribute_writable? true
  end
end
```

### What next?

What you've seen above barely scratches the surface of what Ash can do. In a lot of ways, it will look very similar to other tools that you've seen. If all that you ever used was the above, then realistically you won't see much benefit to using Ash.

Where Ash shines however, is all of the tools that can operate on your resources. You have the ability to extend the framework yourself, and apply consistent design patterns that enable unparalleled efficiency, power and flexibility as your application grows.

#### Get Help

- Check out [ElixirForum](https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum/123)
- Join our [Discord server](https://discord.gg/D7FNG2q)
- Open a [GitHub issue](https://github.com/ash-project/ash/issues/new/choose)

#### Clean up your code that uses Ash?

Creating and using changesets manually can be verbose, and they all look very similar. Luckily, Ash has your back and can generate these for you using Code Interfaces!

Check out the [Code Interface Guide](/documentation/topics/code-interface.md) to derive things like `Helpdesk.Support.Ticket.assign!(representative.id)`

#### Persist your data

See [The AshPostgres getting started guide](https://hexdocs.pm/ash_postgres/get-started-with-postgres.html) to see how to back your resources with Postgres. This is highly recommended, as the Postgres data layer provides tons of advanced capabilities.

#### Add an API

Check out the `AshJsonApi` and `AshGraphql` extensions to effortlessly build APIs around your resources

#### Authorize access and work with users

See the [Policies guide](/documentation/topics/policies.md) for information on how to authorize access to your resources using actors and policies.
