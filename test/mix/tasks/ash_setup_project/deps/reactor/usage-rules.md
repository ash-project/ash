<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Reactor Usage Rules

Reactor is a dynamic, concurrent, dependency-resolving saga orchestrator for Elixir providing saga orchestration, dependency resolution via DAG, concurrent execution, dynamic workflows, and composable DSL.

**Core concepts:** Reactors contain inputs/steps/dependencies. Steps are work units with unique names, dependencies, sync/async execution, compensation/undo capabilities. Arguments define step dependencies and data sources with optional transformations.

**Organization:** Use DSL for static workflows, `Reactor.Builder` for dynamic construction, implement `Reactor.Step` behaviour for custom steps, compose sub-reactors for complex workflows.

## DSL Syntax

```elixir
defmodule MyReactor do
  use Reactor
  
  input :data                           # Define inputs
  
  step :process do                      # Anonymous function step
    argument :data, input(:data)
    run fn %{data: data}, _ -> {:ok, process(data)} end
  end
  
  step :save, MyApp.SaveStep do         # Module step
    argument :processed, result(:process)
  end
  
  return :save                          # Return result
end
```

## Step Implementation

```elixir
defmodule MyApp.Steps.CreateUser do
  use Reactor.Step

  def run(arguments, context, options) do
    {:ok, create_user(arguments.email, arguments.password_hash)}
  end

  def compensate(reason, arguments, context, options) do
    case reason do
      %DBConnection.ConnectionError{} -> :retry  # Retry on connection errors
      _other -> :ok                              # Continue rollback
    end
  end

  def undo(user, arguments, context, options) do
    :ok = delete_user(user)  # Rollback on later failure
  end
end
```

## Return Values & Arguments

**Step returns:** `{:ok, value}`, `{:ok, value, [new_steps]}`, `{:error, reason}`, `:retry`, `{:halt, reason}`

**Arguments:** `argument :name, input(:input)`, `argument :name, result(:step)`, `argument :name, value(static)`

**Transforms:** `argument :id do; source result(:user); transform &(&1.id); end`

## Built-in Steps

**debug** - Log values: `debug :log do; argument :data, result(:step); end`

**map** - Process collections: `map :items do; source input(:list); step :process do; argument :item, element(:items); end; end`

**compose** - Embed reactors: `compose :sub, MyReactor do; argument :data, input(:data); end`

**switch** - Conditional: `switch :type do; on result(:data); matches? &(&1.type == :a) do; step ...; end; default do; step ...; end; end`

**group** - Shared setup: `group :ops do; before_all &setup/3; step ...; end`

**around** - Wrap execution: `around :tx, &with_transaction/4 do; step ...; end`

**collect** - Gather results: `collect :summary do; argument :a, result(:step_a); argument :b, result(:step_b); end`

## Execution Control

**Async:** Steps run async by default. Use `async? false` for sync, `async? fn opts -> condition end` for conditional.

**Guards:** `guard &(&1.user.active?)` or `where fn %{user: user} -> user.plan == :premium end`

**Dependencies:** `wait_for :other_step` for ordering without data flow

**Halting:** Step returns `{:halt, reason}` → `{:halted, state} = Reactor.run(...)` → `Reactor.run(state, %{}, %{})`

**Running:** `Reactor.run(MyReactor, inputs, context, opts)`

## Middleware

```elixir
defmodule MyApp.LoggingMiddleware do
  use Reactor.Middleware

  def init(context), do: {:ok, context}
  def complete(result, context), do: {:ok, result}  
  def error(errors, context), do: :ok
  def event({:run_start, args}, step, context), do: log_step_start(step, args)
end

# Usage in reactor
middlewares do
  middleware MyApp.LoggingMiddleware
  middleware Reactor.Middleware.Telemetry
end
```

## Advanced Features

**Context:** Pass data through context: `def run(arguments, context, options)` where `context[:current_user]`

**Transforms:** Apply to inputs/arguments: `input :date do; transform &Date.from_iso8601!/1; end`

**Retries:** `max_retries 3` with `compensate` returning `:retry`

**Composition:** `compose :sub, SubReactor do; argument :data, result(:prep); end`

## Best Practices

**Error Handling:** Implement `compensate/4` for retryable errors, `undo/4` for rollback. Use specific error types. Work transactionally when possible. Use `Ash.Reactor` steps for Ash apps.

**Performance:** Use `async? false` sparingly, set `batch_size` for maps, use `strict_ordering? false` when order doesn't matter, limit `max_concurrency` for constrained resources.

**Organization:** Create reusable step modules, compose sub-reactors for complex workflows, group related steps, use meaningful names.

**Testing:** Test steps in isolation, test various input combinations and error scenarios, use `async? false` for deterministic tests.

**Debugging:** Use debug steps, add telemetry middleware, use descriptive names, test with `async? false` to simplify.
