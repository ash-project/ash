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
- There aren't side effects or external servies
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
    HelpDesk.Notifications.notify_assignment(agent, ticket)

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

## Reactor Example

Lets implement our complex workflow from Example 3 as a reactor. Reactors are better for modeling complex error states and rollback flows.

```elixir
defmodule HelpDesk.Reactors.ProcessUrgentTicket do
  use Ash.Reactor

  ash do
    default_domain HelpDesk
  end

  # Inputs
  input :ticket_id
  input :priority
  input :changeset_attrs

  # Step 1: Validate external services (outside transaction)
  step :validate_external_services do
    run fn _args, _context ->
      case HelpDesk.ExternalServices.health_check() do
        :ok ->
          {:ok, :services_available}

        {:error, service} ->
          {:error, "External service #{service} unavailable for urgent processing"}
      end
    end
  end


  # Do the following in a transaction
  transaction :update_ticket_transaction, HelpDesk.Ticket do
    # Step 2: Check if ticket is urgent and reserve resources
    step :reserve_urgent_slot do
      argument :priority, input(:priority)
      wait_for :validate_external_services

      run fn %{priority: priority}, _context ->
        if priority == "urgent" do
          case HelpDesk.ResourceManager.reserve_urgent_slot() do
            {:ok, slot_id} ->
              {:ok, %{slot_id: slot_id, status: "urgent_processing", priority_downgraded: false}}

            {:error, :no_slots_available} ->
              # Fallback to high priority
              {:ok, %{slot_id: nil, status: "open", priority_downgraded: true}}
          end
        else
          {:ok, %{slot_id: nil, status: "open", priority_downgraded: false}}
        end
      end

      # Compensate by releasing slot if downstream fails
      compensate fn _reason, %{priority: priority}, _context, _options ->
        if priority == "urgent" do
          # Try to find and release any slot we might have reserved
          case HelpDesk.ResourceManager.find_reserved_slot_for_ticket(input(:ticket_id)) do
            {:ok, slot_id} -> HelpDesk.ResourceManager.release_slot(slot_id)
            _ -> :ok
          end
        end
        :ok
      end
    end

    # Step 3: Update ticket status and processing slot (in transaction)
    update :update_ticket_status, HelpDesk.Ticket, :update do
      initial input(:ticket_id)
      inputs %{
        status: result(:reserve_urgent_slot, [:status]),
        processing_slot_id: result(:reserve_urgent_slot, [:slot_id]),
        priority: fn ->
          if result(:reserve_urgent_slot, [:priority_downgraded]) do
            "high"
          else
            input(:priority)
          end
        end
      }
    end

    return :update_ticket_status

    # Step 4: Create escalation path for urgent tickets
    create :create_escalation, HelpDesk.Escalation, :create do
      inputs %{
        ticket_id: result(:update_ticket_status, [:id]),
        level: value(1),
        escalated_at: value({DateTime, :utc_now, []})
      }

      # Only create escalation if ticket is urgent processing
      where fn args, _context ->
        ticket = result(:update_ticket_status)
        ticket.status == "urgent_processing"
      end

      # Undo by deleting the escalation
      undo_action :destroy
      undo :always
    end

    # Step 5: Create external urgent case
    step :create_external_case do
      argument :ticket, result(:update_ticket_status)
      wait_for :create_escalation

      run fn %{ticket: ticket}, _context ->
        if ticket.status == "urgent_processing" do
          HelpDesk.ExternalServices.create_urgent_case(ticket)
        else
          {:ok, nil}
        end
      end

      # Compensate by canceling external case
      compensate fn _reason, %{ticket: ticket}, _context, _options ->
        if ticket.status == "urgent_processing" && ticket.external_ref do
          case HelpDesk.ExternalServices.cancel_urgent_case(ticket.external_ref) do
            :ok -> :ok
            {:error, _} ->
              HelpDesk.Logger.warn("Failed to cancel external case #{ticket.external_ref}")
              :ok # Don't fail the compensation
          end
        else
          :ok
        end
      end
    end
  end

  # Step 6: Update ticket with external reference
  update :add_external_reference, HelpDesk.Ticket, :add_external_reference do
    initial result(:update_ticket_status)
    inputs %{
      external_ref: fn ->
        case result(:create_external_case) do
          nil -> nil
          external_ref -> "URG-#{result(:update_ticket_status, [:id])}-#{System.system_time(:second)}"
        end
      end
    }

    # Only update if we have an external reference
    where fn _args, _context ->
      result(:create_external_case) != nil
    end
  end

  # Step 7: Send priority downgrade notification (if needed)
  step :notify_priority_downgrade do
    argument :ticket, result(:add_external_reference)
    argument :priority_downgraded, result(:reserve_urgent_slot, [:priority_downgraded])

    run fn %{ticket: ticket, priority_downgraded: priority_downgraded}, _context ->
      if priority_downgraded do
        HelpDesk.Notifications.notify_priority_downgrade(ticket)
      else
        {:ok, :no_notification_needed}
      end
    end
  end

  # Step 8: Update metrics
  step :update_metrics do
    wait_for :notify_priority_downgrade

    run fn _args, _context ->
      HelpDesk.Metrics.increment_urgent_tickets()
      {:ok, :metrics_updated}
    end
  end

  # Step 9: Cleanup reserved slot (always runs, even on failure)
  step :cleanup_slot do
    argument :slot_info, result(:reserve_urgent_slot)
    wait_for :update_metrics

    run fn %{slot_info: %{slot_id: slot_id}}, _context ->
      if slot_id do
        HelpDesk.ResourceManager.release_slot(slot_id)
      else
        {:ok, :no_slot_to_release}
      end
    end
  end

  # Return the final ticket
  return :add_external_reference
end
```

### Usage as an Action

You can use this reactor directly as a generic action:

```elixir
# In your Ticket resource
actions do
  action :process_urgent, :struct do
    constraints instance_of: HelpDesk.Ticket

    argument :ticket_id, :uuid, allow_nil?: false
    argument :priority, :string, allow_nil?: false
    argument :changeset_attrs, :map, default: %{}

    run HelpDesk.Reactors.ProcessUrgentTicket
  end
end
```

### Benefits of the Reactor Approach

1. **Clear Step Isolation**: Each step has a single responsibility and clear inputs/outputs
2. **Automatic Compensation**: Failed steps automatically trigger compensation for completed steps
3. **Dependency Management**: Steps run in optimal order based on data dependencies, not declaration order
4. **Transaction Control**: Fine-grained control over what runs inside vs outside transactions
5. **Error Handling**: Built-in retry and error handling mechanisms
6. **Testability**: Each step can be tested independently

The reactor version provides much better error handling, automatic cleanup, and clearer separation of concerns compared to the hook-based approach.
