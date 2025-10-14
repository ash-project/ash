<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Multi-Step Actions

Actions in Ash allow you to create sophisticated workflows that coordinate multiple changes or processes. Often business logic crosses multiple resources, and we often want it to be transactional. By leveraging action lifecycle hooks, you can build powerful domain-specific operations. This guide will explore how to build and use multi-step actions using a helpdesk example.

For most use cases, hooks are the preferred approach due to their simplicity and tight integration with Ash's action lifecycle. [Reactor](/documentation/topics/advanced/reactor.md) is the comprehensive solution for truly complex orchestration scenarios. Additionally, you can write [generic actions](/documentation/topics/advanced/generic-actions.md) by hand, implementing an action with fully custom code. Reactors can be used as the `run` function for generic actions, giving them first class support in Ash extensions. See [below](#generic-action-example) for an example.

## When to use hooks vs reactors vs generic actions

You should use hooks for most multi-step workflow scenarios as they provide simplicity and leverage Ash's transactional nature. The key decision point is whether you need compensation/rollback across external services:

**Use hooks when:**
- Coordinating changes within Ash resources (leverages database transactions)
- Performing side effects that don't require rollback (logging, notifications)
- Working with external services that don't need compensation logic
- Building small-to-medium complexity workflows

**Use [reactor](/documentation/topics/advanced/reactor.md) when:**
- You need to compensate/undo changes across multiple external services
- Building complex workflows that require sophisticated error handling and rollback logic
- Coordinating long-running processes that span multiple systems

**Use [generic actions](/documentation/topics/actions/generic-actions.md) when:**
- You need a high-level action that works on multiple resources, and reactor or hooks are not fitting
- There aren't side effects or external services
- Short transactional operations that can be understood at a glance

> ### Durable Workflows {: .info}
>
> For durable workflows, we suggest to use Oban. We provide tools to integrate with Oban in [AshOban](https://hexdocs.pm/ash_oban). AshOban supports very specific types of common workflows, like "triggers" that run periodically for resources, and "scheduled actions" which run generic actions on a cron. You should not be afraid to write "standard" Oban jobs and code where possible. Don't bend over backwards trying to fit everything into AshOban.

## Action Lifecycle Hooks

At the core of Ash's multi-step action capability are action lifecycle hooks. These hooks allow you to run code at specific points during an action's execution:

- **before_transaction**: Runs before the transaction is started. Useful for operations that should happen before the transaction, like external API calls.

- **before_action**: Runs in the transaction, before the data layer is called. Perfect for side effects and expensive logic. This hook can be used with changesets and queries.

- **after_action**: Runs in the transaction, after the data layer is called, only if the action is successful. Ideal for transactional side effects that should only happen on success. This hook can be used with changesets and queries.

- **after_transaction**: Runs after the transaction completes, in both success and error cases. Ideal for operations that should happen regardless of the transaction outcome, and for operations that work with external services.

There are other hooks that we won't go into here, as they are rarely used. See the documentation in `Ash.Changeset` for more.

- **around_action**: Runs code both before and after the action logic, within the transaction.
- **around_transaction**: Runs code both before and after the transaction, outside the transaction.

## Examples

Let's explore multi-step actions through a series of increasingly complex examples using a helpdesk system. Each example builds on concepts from the previous ones.

### Example 1: Simple Activity Logging

The simplest multi-step action uses a single hook to perform a transactional effect. Here's a basic example that logs ticket creation by inserting an activity log.

```elixir
defmodule HelpDesk.Changes.LogActivity do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    # Log activity after the ticket is successfully created
    Ash.Changeset.after_action(changeset, fn _changeset, ticket ->
      HelpDesk.ActivityLog.log("Ticket #{ticket.id} created: #{ticket.title}")
      {:ok, ticket}
    end)
  end
end
```

Use it in your Ticket resource:

```elixir
actions do
  create :create do
    accept [:title, :description]
    change HelpDesk.Changes.LogActivity
  end
end
```

### Example 2: Multi-Hook Ticket Assignment

Building on the first example, let's add ticket assignment logic that uses multiple hooks to coordinate a transactional workflow:

```elixir
defmodule HelpDesk.Changes.AssignTicket do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.before_action(&find_and_assign_agent/1)
    |> Ash.Changeset.after_action(&notify_assignment/2)
  end

  defp find_and_assign_agent(changeset) do
    case HelpDesk.AgentManager.find_available_agent() do
      {:ok, agent} ->
        changeset
        |> Ash.Changeset.force_change_attribute(:agent_id, agent.id)
        |> Ash.Changeset.force_change_attribute(:status, "assigned")
        |> Ash.Changeset.put_context(:assigned_agent, agent)

      {:error, reason} ->
        Ash.Changeset.add_error(changeset, "No agents available: #{reason}")
    end
  end

  defp notify_assignment(changeset, ticket) do
    HelpDesk.Notifications.notify_assignment(changeset.context[:agent], ticket)

    {:ok, ticket}
  end
end
```

### Example 3: Complex Workflow with External Services

This example shows a sophisticated workflow that interacts with external services and handles various error conditions:

```elixir
defmodule HelpDesk.Changes.ProcessUrgentTicket do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    changeset
    # uses before_transaction as it communicates with an external service
    # and we don't want to keep a transaction longer than necessary
    |> Ash.Changeset.before_transaction(&validate_external_services/1)
    # Prepare for processing transactionally
    |> Ash.Changeset.before_action(&prepare_urgent_processing/1)
    # Complete the workflow transactionally
    |> Ash.Changeset.after_action(&complete_urgent_workflow/2)
    # Perform success or failure logic after the transaction
    |> Ash.Changeset.after_transaction(&cleanup_and_notify/2)
  end

  defp validate_external_services(changeset) do
    # Check external services before starting transaction
    case HelpDesk.ExternalServices.health_check() do
      :ok ->
        changeset

      {:error, service} ->
        Ash.Changeset.add_error(changeset,
          message: "External service #{service} unavailable for urgent processing")
    end
  end

  defp prepare_urgent_processing(changeset) do
    priority = Ash.Changeset.get_attribute(changeset, :priority)

    if priority == "urgent" do
      # Reserve resources for urgent processing
      case HelpDesk.ResourceManager.reserve_urgent_slot() do
        {:ok, slot_id} ->
          changeset
          |> Ash.Changeset.force_change_attribute(:status, "urgent_processing")
          |> Ash.Changeset.force_change_attribute(:processing_slot_id, slot_id)
          |> Ash.Changeset.put_context(:reserved_slot, slot_id)

        {:error, :no_slots_available} ->
          # Fallback to normal priority with notification
          changeset
          |> Ash.Changeset.force_change_attribute(:priority, "high")
          |> Ash.Changeset.put_context(:priority_downgraded, true)
      end
    else
      changeset
    end
  end

  defp complete_urgent_workflow(_changeset, ticket) do
    if ticket.status == "urgent_processing" do
      # Create escalation path and update external tracking
      with {:ok, _escalation} <- create_escalation_path(ticket),
           {:ok, _external_ref} <- HelpDesk.ExternalServices.create_urgent_case(ticket) do

        # Update ticket with external reference
        ticket
        |> Ash.Changeset.for_update(:add_external_reference, %{
          external_ref: "URG-#{ticket.id}-#{System.system_time(:second)}"
        })
        |> HelpDesk.Ticket.update!()
      else
        {:error, reason} ->
          # Handle errors gracefully
          HelpDesk.Logger.error("Failed to complete urgent workflow for ticket #{ticket.id}: #{inspect(reason)}")
          {:ok, ticket}
      end
    end

    {:ok, ticket}
  end

  defp cleanup_and_notify({:ok, ticket}, changeset) do
    # Success case - perform cleanup and notifications
    # Any more complex cleanup than this should be
    # pushing you to consider reactor and/or oban
    if slot_id = Ash.Changeset.get_context(changeset, :reserved_slot) do
      HelpDesk.ResourceManager.release_slot(slot_id)
    end

    if Ash.Changeset.get_context(changeset, :priority_downgraded) do
      HelpDesk.Notifications.notify_priority_downgrade(ticket)
    end

    HelpDesk.Metrics.increment_urgent_tickets()
    {:ok, ticket}
  end

  defp cleanup_and_notify({:error, _reason} = error, changeset) do
    # Error case - clean up resources
    if slot_id = Ash.Changeset.get_context(changeset, :reserved_slot) do
      HelpDesk.ResourceManager.release_slot(slot_id)
    end

    error
  end

  defp create_escalation_path(ticket) do
    HelpDesk.Escalation
    |> Ash.Changeset.for_create(:create, %{
      ticket_id: ticket.id,
      level: 1,
      escalated_at: DateTime.utc_now()
    })
    |> HelpDesk.Escalation.create()
  end
end
```

These examples demonstrate the progression from simple logging to complex cross-resource workflows. Each example builds on the previous concepts while introducing new patterns and considerations for multi-step actions.

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

If you know that you just want to add a single hook to an action, you can use some of the functions in `Ash.Resource.Change.Builtins`, which are simple shorthands for the above form. For example:

```elixir
create :open_and_assign do
  change after_action(changeset, result, context ->
    ...
  end)
end
```

Notice how the anonymous function takes an extra argument. In the first format above, the `context` value came from `change fn changeset, context ->`, but in this format, it is provided as an argument at the end of the builtin function, i.e `change after_action(changeset, result, context -> .`

## Batch Callbacks

When working with bulk actions (like `Ash.bulk_create/3`, `Ash.bulk_update/3`, etc.), you can optimize your changes by implementing batch callbacks. These allow you to process multiple changesets together, which can be much more efficient than processing them individually.

### Understanding Batch Callbacks

Batch callbacks are optional methods you can implement in your change modules:

- **`batch_change/3`**: Replaces `change/3` for batch operations, receiving a list of changesets
- **`before_batch/3`**: Runs before the batch is sent to the data layer
- **`after_batch/3`**: Runs after the batch completes, receiving changeset-result pairs

> ### Batch Operations and Transactions {: .info}
>
> The batch logic is currently all within transactions. There is no `before_batch_transaction` or `after_batch_transaction` hooks yet.

> ### When are batch callbacks used? {: .warning}
>
> - `batch_change/3` must be defined for `before_batch/3` and `after_batch/3` to be called
> - The exception is `after_batch/3`, which is also called after atomic changes when `atomic/3` is defined
> - `before_batch/3` is ignored when calling changes atomically
> - If you define `batch_change/3`, you can omit `change/3` entirely

### Example: Batch Processing with External API

Here's an example that demonstrates efficient batch processing for ticket creation with external service integration:

```elixir
defmodule HelpDesk.Changes.BatchNotifyExternalSystem do
  use Ash.Resource.Change

  @impl true
  def batch_change(changesets, _opts, _context) do
    # Process all changesets together - you can do expensive setup here
    # that would be wasteful to repeat for each individual changeset
    Enum.map(changesets, &prepare_for_external_notification/1)
  end

  @impl true
  def before_batch(changesets, opts, context) do
    # Validate external service availability before processing the batch
    case HelpDesk.ExternalAPI.health_check() do
      :ok ->
        # Mark all changesets as ready for external notification
        Enum.map(changesets, fn changeset ->
          Ash.Changeset.put_context(changeset, :external_api_ready, true)
        end)

      {:error, reason} ->
        # Add errors to all changesets if external service is down
        Enum.map(changesets, fn changeset ->
          Ash.Changeset.add_error(changeset,
            message: "External API unavailable: #{reason}")
        end)
    end
  end

  @impl true
  def after_batch(changesets_and_results, opts, context) do
    # Efficiently batch notify external system
    notifications =
      changesets_and_results
      |> Enum.filter(fn {changeset, _result} ->
        Ash.Changeset.get_context(changeset, :external_api_ready, false)
      end)
      |> Enum.map(fn {_changeset, result} ->
        %{
          ticket_id: result.id,
          title: result.title,
          created_at: result.inserted_at
        }
      end)

    # Single API call for entire batch instead of one per ticket
    case HelpDesk.ExternalAPI.batch_notify_tickets(notifications) do
      {:ok, _response} ->
        # Return :ok to indicate success
        :ok

      {:error, error} ->
        # You can return individual errors or notifications
        Enum.map(changesets_and_results, fn {_changeset, result} ->
          {:error, Ash.Error.Invalid.exception(
            message: "Failed to notify external system for ticket #{result.id}: #{error}"
          )}
        end)
    end
  end

  defp prepare_for_external_notification(changeset) do
    # Add any metadata needed for external notification
    changeset
    |> Ash.Changeset.put_context(:needs_external_notification, true)
  end
end
```

### Example: Optimized Database Operations

This example shows how to use batch callbacks to optimize database operations:

```elixir
defmodule HelpDesk.Changes.BatchAssignAgents do
  use Ash.Resource.Change

  @impl true
  def before_batch(changesets, _opts, _context) do
    # Pre-load all available agents once for the entire batch
    available_agents =
      HelpDesk.Agent
      |> Ash.Query.filter(status == "available")
      |> Ash.Query.sort(:workload)
      |> Ash.read!()

    # Distribute agents across the batch
    {assigned_changesets, _remaining_agents} =
      Enum.map_reduce(changesets, available_agents, fn changeset, [agent | rest] ->
        updated_changeset =
          changeset
          |> Ash.Changeset.change_attribute(:agent_id, agent.id)
          |> Ash.Changeset.change_attribute(:status, "assigned")
          |> Ash.Changeset.put_context(:assigned_agent, agent)

        {updated_changeset, rest ++ [%{agent | workload: agent.workload + 1}]}
      end)

    assigned_changesets
  end

  @impl true
  def after_batch(changesets_and_results, _opts, _context) do
    # Batch update agent workloads
    agent_updates =
      changesets_and_results
      |> Enum.map(fn {changeset, _result} ->
        Ash.Changeset.get_context(changeset, :assigned_agent)
      end)
      |> Enum.filter(& &1)
      |> Enum.group_by(& &1.id)
      |> Enum.map(fn {agent_id, assignments} ->
        %{id: agent_id, workload_increment: length(assignments)}
      end)

    # Single bulk operation to update all agent workloads
    HelpDesk.Agent
    |> Ash.bulk_update(:increment_workload, agent_updates)

    :ok
  end

  # Define batch_change/3 to enable batch callbacks
  @impl true
  def batch_change(changesets, _opts, _context) do
    # The actual changeset modifications happen in before_batch
    # This just returns the changesets as-is
    changesets
  end
end
```

### Example: Conditional Batch Processing

You can control when batch callbacks are used with the `batch_callbacks?/3` callback:

```elixir
defmodule HelpDesk.Changes.ConditionalBatchProcessing do
  use Ash.Resource.Change

  @impl true
  def batch_callbacks?(changesets, opts, context) do
    # Only use batch processing for large batches
    length(changesets) >= 10
  end

  @impl true
  def change(changeset, opts, context) do
    # This runs for individual changes or small batches
    changeset
    |> perform_individual_processing(opts, context)
  end

  @impl true
  def batch_change(changesets, opts, context) do
    # This runs for large batches (10+ items)
    changesets
    |> perform_optimized_batch_processing(opts, context)
  end

  defp perform_individual_processing(changeset, _opts, _context) do
    # Simple processing for individual items
    changeset
  end

  defp perform_optimized_batch_processing(changesets, _opts, _context) do
    # Optimized processing for large batches
    changesets
  end
end
```

### Best Practices for Batch Callbacks

1. **Use batch callbacks for expensive operations**: Database queries, external API calls, file I/O
2. **Keep individual operations fast**: If your change is already fast, batch callbacks may not be worth the complexity
3. **Handle errors gracefully**: Return appropriate error tuples from `after_batch/3` when things go wrong
4. **Test both paths**: Ensure your change works correctly both individually and in batches


## Generic Action Example

```elixir
# Define a plain-old elixir module/function to express the action
defmodule HelpDesk.Actions.AssignTicket do
  def run(input, context) do
    with {:ok, agent} <- HelpDesk.AgentManager.find_available_agent(),
         {:ok, ticket} <- HelpDesk.get_ticket_by_id(input.arguments.ticket_id),
         {:ok, ticket} <- HelpDesk.update_ticket(ticket, %{agent_id: agent.id, status: :assigned}, actor: input.actor)
         :ok <- Helpdesk.Notifications.notify_assignment(agent, ticket)
    end
  end
end

# Invoke the action from Resource
actions do
  action :assign_to_available_agent do
    transaction? true
    argument :ticket_id, :uuid
    run HelpDesk.Actions.AssignTicket
  end
end
```
