# Complex & Multi-Step Actions

Multi-step actions in Ash allow you to create sophisticated workflows that coordinate multiple changes or processes together. Often business logic crosses multiple resources, and we often want it to be transcational. By leveraging action lifecycle hooks, you can build powerful domain-specific operations. This guide will explore how to build and use multi-step actions using a helpdesk example. In some cases, hooks can grow to a point where they are unwieldy. You should use hooks for small-to-medium complexity workflows for simplicity's sake. If you need more, look into [reactor](/documentation/topics/advanced/reactor.md), which is the "big kahuna" of orchestrating complex multi-step workflows.

> ### Durable Workflows {: .info}
>
> For durabile workflows, we suggest to use Oban. We provide tools to integrate with oban in [AshOban](hexdocs.pm/ash_oban). AshOban supports very specific types of common workflows, like "triggers" that run periodically for resources, and "scheduled actions" which run generic actions on a cron. You should not be afraid to write "standard" oban jobs and code where possible. Don't bend over backwards trying to fit everything into AshOban.

## Action Lifecycle Hooks

At the core of Ash's multi-step action capability are action lifecycle hooks. These hooks allow you to run code at specific points during an action's execution:

- **before_transaction**: Runs before the transaction is started. Useful for operations that should happen before the transaction, like external API calls.

- **before_action**: Runs in the transaction, before the data layer is called. Perfect for side effects and expensive logic. This hook can be used with changesets and queries.

- **after_action**: Runs in the transaction, after the data layer is called, only if the action is successful. Ideal for transactional side effects that should only happen on success. This hook can be used with changesets and queries.

- **after_transaction**: Runs after the transaction completes, in both success and error cases. Ideal for operations that should happen regardless of the transaction outcome, and for operations that work with external services.

There are other hooks that we won't go into here, as they are rarely used. See the documentation in `Ash.Changeset` for more.

- **around_action**: Runs code both before and after the action logic, within the transaction.
- **around_transaction**: Runs code both before and after the transaction, outside the transaction.

### Creating a Custom Change with Hooks

Let's see how to implement hooks using a custom change module. Here's an example for a helpdesk ticket system:

```elixir
defmodule HelpDesk.Changes.AssignTicket do
  use Ash.Resource.Change

  @impl true
  def init(opts) do
    # Validate the options
    # This is run at compile time
    {:ok, opts}
  end

  @impl true
  def change(changeset, opts, _context) do
    # Add a before_action hook to perform the assignment logic
    Ash.Changeset.before_action(changeset, fn changeset ->
      # Find an available agent
      case HelpDesk.AgentManager.find_available_agent() do
        {:ok, agent} ->
          # Assign the agent to the ticket
          changeset
          |> Ash.Changeset.change_attribute(:agent_id, agent.id)
          |> Ash.Changeset.change_attribute(:status, "assigned")
          |> Ash.Changeset.put_context(:assigned_agent, agent)

        {:error, reason} ->
          # Add an error if no agent is available
          Ash.Changeset.add_error(changeset, message: "No agents available: #{reason}")
      end
    end)
    |> Ash.Changeset.after_action(fn changeset, result ->
      # Notify the agent after successful assignment
      if agent = Ash.Changeset.get_context(changeset, :assigned_agent) do
        HelpDesk.Notifications.notify_agent(agent, result)
      end

      # Return the result
      {:ok, result}
    end)
  end
end
```

You would use this change in your Ticket resource like this:

```elixir
actions do
  create :open_and_assign do
    accept [:title, :description, :priority]

    # Apply our custom change that adds the hooks
    change {HelpDesk.Changes.AssignTicket, []}
  end
end
```

### Shortcuts for hooks

There are multiple ways to add hooks to actions. What we showed above, defining an `Ash.Resource.Change` module, and using the functions in `Ash.Changeset` to add callbacks to the module is the most organized & idiomatic way. However, you can also use the following techniques as shorthand. In general, prefer to write your changes into modules to keep your resources clean and keep compile times down (more smaller modules is typically better than fewer larger modules).

#### Anonymous Function Changes

Here we use an anonymous function change, which is a shorthand for defining a change module with a single function. Just like in the `change/3` function, you can add hooks to the changeset.

```elixir
create :open_and_assign do
  change fn changeset, context ->
    Ash.Changeset.after_action(changeset, fn changeset, result ->
      ...
    end)
  end
end
```

#### Builtin Hook Changes

If you know that you just want to add a single hook to an action, you can use some of the functions in `Ash.Resource.Change.Builtins`, which are simple shorhands for the above form. For example:

```elixir
create :open_and_assign do
  change after_action(changeset, result, context ->
    ...
  end)
end
```

Notice how the anonymous function takes an extra argument. In the first format above, the `context` value came from `change fn changeset, context ->`, but in this format, it is provided as an argument at the end of the builtin function, i.e `change after_action(changeset, result, context -> .`
