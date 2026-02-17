<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Building Async Workflows

In this tutorial, you'll learn how to build efficient concurrent workflows that take advantage of Reactor's dependency resolution and async execution capabilities.

## What you'll build

A data processing pipeline that:
1. Fetches user data from multiple sources concurrently
2. Processes different data types in parallel
3. Aggregates results efficiently
4. Handles mixed sync/async requirements
5. Optimizes performance with proper concurrency control

## You'll learn

- How Reactor's concurrency model works
- When to use async vs sync execution
- How to optimize workflow performance
- Managing dependencies for maximum parallelisation
- Controlling concurrency limits and resource usage

## Prerequisites

- Complete the [Getting Started tutorial](01-getting-started.md)
- Complete the [Error Handling tutorial](02-error-handling.md)
- Basic understanding of Elixir processes

## Step 1: Set up the project

If you don't have a project from the previous tutorials:

```bash
mix igniter.new reactor_tutorial --install reactor
cd reactor_tutorial
```

## Step 2: Understanding Reactor's concurrency model

Reactor runs steps **asynchronously by default**:

- **Independent steps run in parallel** - Steps with no dependencies execute simultaneously
- **Dependencies control execution order** - Steps wait for their dependencies to complete
- **Automatic task management** - Reactor manages Elixir tasks and supervision
- **Configurable concurrency** - Control how many steps run at once

### Async vs Sync execution

```elixir
# Async (default) - runs in a separate task
step :fetch_data do
  async? true  # This is the default
  run &fetch_from_api/1
end

# Sync - runs in the main process
step :critical_operation do
  async? false  # Forces synchronous execution
  run &update_database/1
end
```

## Step 3: Create simple data operations

Let's create some data operations that will run concurrently. Create `lib/data_sources.ex`:

```elixir
defmodule DataSources do
  def fetch_user_profile(user_id) do
    Process.sleep(200)
    
    {:ok, %{
      id: user_id,
      name: "User #{user_id}",
      email: "user#{user_id}@example.com"
    }}
  end

  def fetch_user_preferences(user_id) do
    Process.sleep(150)
    
    {:ok, %{
      user_id: user_id,
      theme: "light",
      language: "en"
    }}
  end

  def fetch_user_activity(user_id) do
    Process.sleep(300)
    
    {:ok, %{
      user_id: user_id,
      last_login: DateTime.utc_now(),
      login_count: 42
    }}
  end
end
```

## Step 4: Build a concurrent data reactor

Now let's build a reactor that fetches data concurrently. Create `lib/async_user_data_reactor.ex`:

```elixir
defmodule AsyncUserDataReactor do
  use Reactor

  input :user_id

  # These steps have no dependencies on each other, so they run in parallel
  step :fetch_profile do
    argument :user_id, input(:user_id)
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_profile(user_id)
    end
  end

  step :fetch_preferences do
    argument :user_id, input(:user_id)  
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_preferences(user_id)
    end
  end

  step :fetch_activity do
    argument :user_id, input(:user_id)
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_activity(user_id)
    end
  end

  # This step waits for all the fetch steps to complete
  step :aggregate_data do
    argument :profile, result(:fetch_profile)
    argument :preferences, result(:fetch_preferences)
    argument :activity, result(:fetch_activity)
    
    run fn args, _context ->
      user_data = %{
        profile: args.profile,
        preferences: args.preferences,
        activity: args.activity,
        summary: "User #{args.profile.id} has #{args.activity.login_count} logins"
      }
      {:ok, user_data}
    end
  end

  return :aggregate_data
end
```

## Step 5: Test the concurrent execution

Let's test our reactor to see the difference between concurrent and sequential execution:

```bash
iex -S mix
```

```elixir
# Test the concurrent execution
start_time = :erlang.monotonic_time(:millisecond)

{:ok, result} = Reactor.run(AsyncUserDataReactor, %{user_id: 123})

end_time = :erlang.monotonic_time(:millisecond)
duration = end_time - start_time

IO.puts("Completed in #{duration}ms")
IO.inspect(result.summary)
```

## Step 6: Understanding the execution flow

The reactor completes in about **300ms** instead of 650ms (200+150+300) because:

1. All three fetch steps run **concurrently** (they only depend on input)
2. `:aggregate_data` waits for all three to complete
3. Total time is limited by the slowest operation (300ms) not the sum

Compare with synchronous execution:

```elixir
# Force all steps to run synchronously
{:ok, result} = Reactor.run(AsyncUserDataReactor, %{user_id: 123}, %{}, async?: false)
```

This takes the full 650ms because each step runs sequentially.

## Step 7: Controlling async behavior

You can control which steps run synchronously when needed:

```elixir
defmodule SyncVsAsyncReactor do
  use Reactor

  input :user_id

  # I/O operations - keep async (default)
  step :fetch_profile do
    argument :user_id, input(:user_id)
    # async? true  # This is the default
    run fn %{user_id: user_id}, _context ->
      DataSources.fetch_user_profile(user_id)
    end
  end

  # CPU-intensive work - tune reactor concurrency as needed
  step :process_data do
    argument :profile, result(:fetch_profile)
    # async? true is the default - adjust reactor max_concurrency instead
    
    run fn %{profile: profile}, _context ->
      Process.sleep(100)
      {:ok, Map.put(profile, :processed, true)}
    end
  end

  return :process_data
end
```

## What you learned

You now understand Reactor's concurrency model:

- **Steps run async by default** - Enables automatic parallelisation
- **Dependencies determine execution order** - Independent steps run concurrently
- **Tune concurrency for your workload** - Adjust limits based on system capacity
- **Performance optimisation** - Balance concurrency with system resources
- **Concurrency control** - Manage resource usage with limits

### Performance guidelines:

- **I/O operations** → Generally benefit from high concurrency
- **CPU-intensive work** → Tune concurrency to match CPU cores and workload
- **Resource limits** → Set concurrency limits based on system capacity

## What's next

Now that you understand concurrency, you're ready for advanced workflow patterns:

- **[Composition](04-composition.md)** - Build complex workflows with sub-reactors
- **[Recursive Execution](05-recursive-execution.md)** - Advanced iterative patterns
- **[Testing Strategies](documentation/how-to/testing-strategies.md)** - Test concurrent workflows effectively

## Common issues

**Steps aren't running in parallel**: Check for hidden dependencies in arguments - each argument creates a dependency

For comprehensive performance and concurrency troubleshooting, see [Performance Optimization](documentation/how-to/performance-optimization.md#troubleshooting).

Happy building concurrent workflows! ⚡
