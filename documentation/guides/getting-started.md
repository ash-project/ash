# Getting Started

The first step is to decide if you're building a phoenix application or not. Phoenix is an extremely high quality web framework, and is the suggested pairing with Ash if you expect to be building a web front end, or an API. For this guide, we assume that elixir has already been installed. We will be using a "helpdesk" example throughout the documentation, so if you want to play along with your own application, you'll need to replace various names.

## Installing With Phoenix

Install the Phoenix installation archive and then create a new Phoenix application. Be sure to look over the options available with `mix help phx.new`, and visit the phoenix [Phoenix Documentation](https://www.phoenixframework.org/) for more information.

```bash
mix archive.install hex phx_new
mix phx.new cool_desk --live
```

## Installing Without Phoenix

Create a new application. Be sure to look aver the options available with `mix help new`.

```bash
mix new cool_desk
```

## Adding Ash

1. First off, add Ash as a dependency. In `mix.exs`, add
`{:ash, "~> 1.52.0-rc.0"}` to your dependencies.

2. Next, add an API. Create  `lib/cool_desk/tickets/tickets.ex`, with the following contents:

    ```elixir
    defmodule CoolDesk.Tickets do
      use Ash.Api, otp_app: :cool_desk
    end
    ```

3. Add a Registry. A registry is where you list the resources that a given Api has access to. Create `lib/cool_desk/tickets/registry.ex` with the following contents:

    ```elixir
    defmodule CoolDesk.Tickets.Registry do
      use Ash.Registry,
        extensions: [Ash.Registry.ResourceValidations]

      entries do
        # We will list our resources here
      end
    end
    ```

4. Configure your application. Add the following to `config/config.exs`.

    ```elixir
    # Configure the list of APIs in your application.
    config :cool_desk, ash_apis: [
      CoolDesk.Tickets
    ] 

    # Configure the registry to be used for your first API
    # Storing this in configuration instead of the API improves
    # compile times.
    config :my_app, CoolDesk.Tickets,
      resources: [
        registry: CoolDesk.Tickets.Registry
      ]
    ```

5. Define your first resource. Place it at `lib/cool_desk/tickets/resources/ticket.ex`.

    ```elixir
    defmodule CoolDesk.Tickets.Ticket do
      use Ash.Resource, data_layer: Ash.DataLayer.Ets
      # For now, we will use the `Ets` data layer, which is builtin and is very useful for quick prototyping. 
      # Data is stored in memory and will be lost when the app restarts.

      attributes do
        # We generally recommend using UUIDs, but you can
        # also use `integer_primary_key :id`, or simply define
        # your own with:
        #
        # `attribute :name, :type, primary_key?: true`
        uuid_primary_key :id

        # Add the attributes of your resource. For example,
        # A "User" might have a `username` and an `email` attribute,
        # or a "BlogPost" might have a `body` attribute
        attribute :subject, :string
      end
    end
    ```

6. Add your resource to the registry that you created

    ```elixir
    entries do
      entry Cooldesk.Tickets.Ticket
    end
    ```

7. Resources are static descriptions of behavior, and don't do anything on their own. To give them functionality, we must first add [actions](../concepts/actions.md), and then we will "invoke" those actions through an Api module. The simplest way to start is to use the `defaults` option to create the four default actions. Add the following to your resource.

   ```elixir
   actions do
     defaults [:create, :read, :update, :destroy]
   end
   ```

8. Try it out in iex (run your app with `iex -S mix`):

    Create two tickets:

    ```elixir
    CoolDesk.Tickets.Ticket
    |> Ash.Changeset.for_create(
      :create, 
      %{subject: "My computer fell into Mount Doom."}
    )
    |>  CoolDesk.Tickets.create!()
    ```

    List your tickets 

    ```elixir
    CoolDesk.Tickets
    |> Ash.Query.for_read(:read)
    |> CoolDesk.Tickets.read!()
    ```