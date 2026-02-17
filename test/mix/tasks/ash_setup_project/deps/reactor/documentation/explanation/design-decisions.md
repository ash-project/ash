<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Design Decisions

This guide explains the reasoning behind Reactor's key design choices, helping you understand why the framework works the way it does and how these decisions benefit different use cases.

## Why Dependency-Based Execution?

### The Decision

Reactor uses argument dependencies to determine execution order rather than requiring explicit step sequencing. When a step declares `argument :user_id, result(:fetch_user)`, it automatically creates a dependency that ensures `fetch_user` runs before the current step.

### Reasoning Behind the Choice

**Traditional Sequential Approach:**
```elixir
# Sequential workflow - explicit ordering
step_1 → step_2 → step_3 → step_4
```

**Reactor's Dependency Approach:**
```elixir
# Dependency-driven workflow - automatic ordering
defmodule UserWorkflow do
  use Reactor
  
  step :fetch_user do
    argument :user_id, input(:user_id)
  end
  
  step :fetch_profile do
    argument :user_id, input(:user_id)  # Can run concurrently with fetch_user
  end
  
  step :merge_data do
    argument :user, result(:fetch_user)      # Waits for fetch_user
    argument :profile, result(:fetch_profile) # Waits for fetch_profile
  end
end
```

### Benefits and Trade-offs

**Benefits:**
- **Automatic Concurrency**: Steps with no shared dependencies run simultaneously
- **Natural Data Flow**: Dependencies express actual data relationships
- **Dynamic Workflows**: Steps can emit new steps with automatically resolved dependencies
- **Composition**: Sub-workflows integrate seamlessly without manual coordination

**Trade-offs:**
- **Learning Curve**: Developers must think in terms of data dependencies vs linear execution
- **Debugging Complexity**: Execution order determined at runtime, may require visualisation tools
- **Graph Overhead**: Dependency analysis and memory overhead for graph structures

## Saga Pattern with Three-Tier Error Handling

### The Decision

Reactor implements the saga pattern with a three-tier error handling approach: retry → compensation → undo, rather than traditional distributed transactions or simple error propagation.

### Why Saga Pattern?

**Distributed Transaction Limitations:**
- Require all participants to support two-phase commit
- Poor availability during failures
- Don't work across different system types (databases, APIs, file systems)

**Saga Pattern Benefits:**
- Works with any system type
- Graceful degradation during failures
- Business logic integrated into error handling
- Natural fit for microservice architectures

### Three-Tier Error Handling

**Level 1: Retry Logic**
```elixir
def compensate(%NetworkTimeoutError{}, _arguments, context, _step) do
  # Exponential backoff - Reactor handles max_retries automatically
  delay = :math.pow(2, context.current_try) * 1000
  Process.sleep(delay)
  :retry
end
```

**Level 2: Compensation (Smart Recovery)**
```elixir
def compensate(%PaymentGatewayError{backup_gateway: backup}, arguments, _context, _step) do
  # Try backup payment gateway
  case BackupGateway.charge(arguments.amount, arguments.card) do
    {:ok, charge} -> {:continue, charge}
    {:error, _} -> :ok  # Proceed to undo
  end
end
```

**Level 3: Undo (Rollback)**
```elixir
def undo(created_user, _arguments, _context, _step) do
  # Remove the user that was successfully created
  UserDatabase.delete(created_user.id)
  :ok
end
```

### Design Philosophy

**Step-Level Autonomy with Global Coordination:**
- Each step decides how to handle its own failures (local knowledge)
- Global system coordinates rollback when needed (system consistency)
- Business logic stays close to the step that understands it

**Multiple Recovery Paths:**
- Temporary issues handled by retry
- Business logic errors handled by compensation
- System consistency maintained by undo
- Clear separation between technical failures and business logic failures

## DSL vs Programmatic APIs

### The Decision

Reactor provides both a declarative DSL (using Spark) and a programmatic Builder API, rather than choosing just one approach.

### Rationale for Dual Approach

**DSL Strengths - Static Workflows:**
```elixir
defmodule OrderProcessing do
  use Reactor
  
  input :order_id
  input :payment_info
  
  step :validate_order do
    argument :order_id, input(:order_id)
    run &OrderValidator.validate/2
  end
  
  step :process_payment do
    argument :order, result(:validate_order)
    argument :payment_info, input(:payment_info)
    run &PaymentProcessor.charge/2
    compensate &PaymentProcessor.refund/4
  end
end
```

**Builder API Strengths - Dynamic Workflows:**
```elixir
def build_dynamic_workflow(config) do
  reactor = Reactor.Builder.new()
  
  # Add steps based on runtime configuration
  Enum.reduce(config.processing_steps, reactor, fn step_config, reactor ->
    {:ok, reactor} = Reactor.Builder.add_step(reactor, step_config.name, step_config.impl)
    reactor
  end)
end
```

### When to Use Each

**Use DSL When:**
- Workflow structure is known at compile time
- You want declarative, self-documenting code
- Static analysis and validation is important

**Use Builder API When:**
- Workflow structure depends on runtime data
- Building workflows programmatically
- Integrating with code generation tools

**Both approaches produce identical runtime structures and can be composed together.**

## Concurrency by Default

### The Decision

Steps run asynchronously by default (`async?: true`) with sophisticated concurrency management and deadlock prevention.

### Why Async by Default?

**Modern Computing Reality:**
- Multi-core processors are standard
- I/O operations dominate many workflows
- Network latency is orders of magnitude higher than CPU operations

**Performance Benefits:**
```elixir
# Without concurrency: 300ms total
fetch_user(100ms) → fetch_preferences(100ms) → fetch_history(100ms)

# With concurrency: ~100ms total  
fetch_user(100ms) ┐
fetch_preferences(100ms) ├→ merge_results
fetch_history(100ms) ┘
```

### Sophisticated Concurrency Management

**Deadlock Prevention:**
```elixir
# Automatic handling of nested reactor scenarios
defmodule ParentReactor do
  use Reactor

  compose :process_batch, ChildReactor do
    # When concurrency pool exhausted, composed reactor runs synchronously
    # Automatically detects and prevents deadlock
  end
end

# Run with shared concurrency pool
Reactor.run(ParentReactor, %{}, max_concurrency: 8)
```

**Resource Management:**
- Shared concurrency pools across reactor hierarchies
- Automatic resource acquisition and release
- Process monitoring and cleanup

### Design Philosophy

**Performance by Default:**
- Optimal performance without developer intervention
- Scales automatically with system capabilities
- Simple mental model: "declare dependencies, get concurrency"

**Safety and Reliability:**
- Automatic deadlock detection and prevention
- Resource limits prevent system exhaustion
- Explicit opt-out for special cases (`async?: false`)

## Integration with Spark/Ash Ecosystem

### The Decision

Reactor is built on the Spark library and designed to integrate seamlessly with the Ash ecosystem, while still being a standalone workflow engine that works with any Elixir application.

### Why Build on Spark?

**Shared DSL Infrastructure:**
```elixir
# Consistent DSL patterns across Ash ecosystem
defmodule MyResource do
  use Ash.Resource
  
  actions do
    read :list_users
    create :create_user
  end
end

defmodule MyReactor do
  use Reactor  # Same DSL foundation
  
  step :list_users
  step :create_user
end
```

### Ecosystem Integration Benefits

**Complementary Capabilities:**
- **Ash**: Data layer with resources, actions, policies
- **Reactor**: Workflow orchestration and business process management
- **Phoenix**: Web interface and real-time communication

**Shared Patterns:**
- Compatible concurrency models
- Similar error handling approaches
- Consistent configuration and tooling

**Natural Composition:**
```elixir
defmodule OrderWorkflow do
  use Reactor
  
  step :create_order do
    run fn args, _context ->
      # Integrate with Ash actions
      args.order_params
      |> Order.create()
      |> case do
        {:ok, order} -> {:ok, order}
        {:error, changeset} -> {:error, changeset}
      end
    end
  end
end
```

### Architectural Benefits

**Learning Transfer:**
- Developers familiar with Ash can quickly learn Reactor
- Shared concepts, patterns, and tooling
- Reduced learning curve

**Future Evolution:**
- Can evolve together with Ash ecosystem
- Shared infrastructure improvements
- Coordinated feature development

### Design Constraints and Benefits

**Constraints Accepted:**
- Dependency on Spark
- Need to maintain compatibility with Ash patterns

**Benefits Gained:**
- Massive reduction in DSL implementation complexity
- Automatic compatibility with existing Ash tooling
- Strong ecosystem alignment and developer experience

## Summary

These design decisions work together to create a framework that:

1. **Maximises Performance**: Through dependency-based concurrency and async-by-default execution
2. **Enables Complex Workflows**: Through saga patterns and sophisticated error handling
3. **Provides Developer Choice**: Through dual DSL/programmatic APIs
4. **Integrates Naturally**: Through Spark/Ash ecosystem alignment
5. **Scales Gracefully**: Through composable patterns and resource management

Understanding these decisions helps you make better choices when designing workflows and contributes to effective use of Reactor in complex applications.
