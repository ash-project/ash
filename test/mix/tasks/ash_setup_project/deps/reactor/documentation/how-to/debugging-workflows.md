<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# How to Debug Reactor Workflows

## Problem

Your reactor isn't behaving as expected and you need effective techniques to troubleshoot and debug workflow execution, identify bottlenecks, and understand what's happening during execution.

## Solution Overview

This guide shows you debugging techniques, tools, and patterns for identifying and fixing issues in your reactors using Reactor's built-in debugging features, telemetry, visualization tools, and error analysis.

## Prerequisites

- Understanding of Reactor basics
- Experience building reactors
- Basic knowledge of Elixir debugging techniques

## Debugging Strategies

### 1. Using Debug Steps

The simplest way to understand what's happening in your reactor is to add debug steps that log intermediate values.

#### Basic Debug Step

```elixir
defmodule UserProcessingReactor do
  use Reactor

  input :user_data

  step :validate_user, MyApp.Steps.ValidateUser do
    argument :user_data, input(:user_data)
  end

  debug :check_validation do
    argument :user, result(:validate_user)
    level :info
  end

  step :create_user, MyApp.Steps.CreateUser do
    argument :user, result(:validate_user)
  end

  debug :check_creation do
    argument :result, result(:create_user)
  end
end
```

#### Debug Output

Debug steps log comprehensive information about arguments, context, and options:

```
[info] # Debug information for step `:check_validation`.

## Arguments
%{
  user: %{
    email: "user@example.com",
    name: "John Doe",
    validated: true
  }
}

## Context
%{
  concurrency_key: #Reference<0.123.456.789>,
  current_step: :check_validation
}

## Options
[]
```

### 2. Synchronous Execution for Debugging

When debugging, disable async execution to get predictable, deterministic behavior:

```elixir
defmodule DebuggingTest do
  test "debug complex workflow" do
    inputs = %{user_data: sample_data()}
    
    result = Reactor.run(UserProcessingReactor, inputs, async?: false)
    
    case result do
      {:ok, user} -> 
        IO.puts("Success: #{inspect(user)}")
      {:error, errors} -> 
        IO.puts("Failed: #{inspect(errors)}")
    end
  end
end
```

### 3. Visual Workflow Debugging

Generate visual diagrams to understand reactor structure and flow:

#### Creating Mermaid Diagrams

Use the built-in Mix task to generate visual diagrams:

```bash
# Generate basic diagram
mix reactor.mermaid UserProcessingReactor

# Include descriptions and expand sub-reactors
mix reactor.mermaid UserProcessingReactor --describe --expand

# Save to specific file
mix reactor.mermaid UserProcessingReactor --output debug_flow.mmd

# Display diagram for copy-pasting into Mermaid Live
mix reactor.mermaid UserProcessingReactor --format copy

# Generate direct Mermaid Live Editor URL
mix reactor.mermaid UserProcessingReactor --format url
```

Or generate diagrams programmatically:

```elixir
defmodule DebugHelpers do
  def visualize_reactor(reactor_module, filename \\ "reactor_debug.mmd") do
    {:ok, diagram} = Reactor.Mermaid.to_mermaid(reactor_module, 
      describe?: true,
      expand?: true
    )
    
    File.write!(filename, diagram)
    IO.puts("Diagram saved to #{filename}")
    IO.puts("View at: https://mermaid.live/edit")
  end
end

DebugHelpers.visualize_reactor(UserProcessingReactor)
```

#### Understanding the Diagram

The generated diagram shows:
- Input dependencies
- Step execution order
- Data flow between steps
- Conditional branches and loops

### 4. Telemetry-Based Debugging

Set up telemetry handlers to monitor reactor execution in real-time:

#### Setting Up Telemetry

```elixir
defmodule MyApp.ReactorTelemetry do
  def setup_debugging() do
    events = [
      [:reactor, :run, :start],
      [:reactor, :run, :stop],
      [:reactor, :step, :run, :start],
      [:reactor, :step, :run, :stop]
    ]
    
    :telemetry.attach_many(
      "reactor-debug",
      events,
      &handle_event/4,
      nil
    )
  end
  
  def handle_event([:reactor, :run, :start], _measurements, metadata, _config) do
    IO.puts("🚀 Starting reactor: #{metadata.reactor.id}")
  end
  
  def handle_event([:reactor, :run, :stop], measurements, metadata, _config) do
    duration_ms = measurements.duration / 1_000_000
    
    case metadata.outcome do
      :ok -> 
        IO.puts("✅ Reactor completed in #{duration_ms}ms")
      :error -> 
        IO.puts("❌ Reactor failed in #{duration_ms}ms")
      :halt ->
        IO.puts("⏸️ Reactor halted in #{duration_ms}ms")
    end
  end
  
  def handle_event([:reactor, :step, :run, :start], _measurements, metadata, _config) do
    IO.puts("   🔄 Starting step: #{metadata.step.name}")
  end
  
  def handle_event([:reactor, :step, :run, :stop], measurements, metadata, _config) do
    duration_ms = measurements.duration / 1_000_000
    
    case metadata.outcome do
      :ok -> 
        IO.puts("   ✅ Step #{metadata.step.name} completed in #{duration_ms}ms")
      :error -> 
        IO.puts("   ❌ Step #{metadata.step.name} failed in #{duration_ms}ms")
      :retry ->
        IO.puts("   🔄 Step #{metadata.step.name} retrying after #{duration_ms}ms")
    end
  end
end
```

#### Using Telemetry in Your Reactor

```elixir
defmodule MyReactor do
  use Reactor

  middlewares do
    middleware Reactor.Middleware.Telemetry
  end

  # ... steps
end

# Set up debugging and run
MyApp.ReactorTelemetry.setup_debugging()
Reactor.run(MyReactor, inputs)
```

### 5. Error Analysis and Debugging

Understand and debug errors using Reactor's structured error system:

#### Analyzing Reactor Errors

```elixir
defmodule ErrorAnalyzer do
  def debug_error(error) do
    IO.puts("🔍 Analyzing reactor error...")
    
    case error do
      %Reactor.Error.Invalid.RunStepError{} = step_error ->
        IO.puts("❌ Step execution failed:")
        IO.puts("   Step: #{step_error.step.name}")
        IO.puts("   Error: #{inspect(step_error.error)}")
        debug_step_context(step_error.step)
        
      %Reactor.Error.Invalid.MissingArgumentError{} = arg_error ->
        IO.puts("❌ Missing argument:")
        IO.puts("   Argument: #{arg_error.argument.name}")
        IO.puts("   Step: #{arg_error.step.name}")
        debug_dependency_chain(arg_error.step, arg_error.argument)
        
      %Reactor.Error.Validation.MissingReturnError{} ->
        IO.puts("❌ No return value specified for reactor")
        IO.puts("💡 Add a return statement to your reactor")
        
      errors when is_list(errors) ->
        IO.puts("❌ Multiple errors occurred:")
        Enum.with_index(errors, 1)
        |> Enum.each(fn {err, idx} ->
          IO.puts("   #{idx}. #{inspect(err)}")
        end)
        
      _ ->
        IO.puts("❌ Unknown error: #{inspect(error)}")
    end
  end
  
  defp debug_step_context(step) do
    IO.puts("   Arguments:")
    Enum.each(step.arguments, fn arg ->
      IO.puts("     - #{arg.name}: #{inspect(arg.source)}")
    end)
  end
  
  defp debug_dependency_chain(step, argument) do
    IO.puts("💡 Check if the source step completed successfully:")
    
    case argument.source do
      %Reactor.Template.Result{name: source_step} ->
        IO.puts("   Source step: #{source_step}")
        IO.puts("   Add a debug step after #{source_step} to verify its output")
        
      %Reactor.Template.Input{name: input_name} ->
        IO.puts("   Missing input: #{input_name}")
        IO.puts("   Ensure this input is provided when running the reactor")
        
      _ ->
        IO.puts("   Source: #{inspect(argument.source)}")
    end
  end
end

# Usage
case Reactor.run(MyReactor, inputs) do
  {:ok, result} -> 
    IO.puts("Success: #{inspect(result)}")
  {:error, errors} ->
    Enum.each(List.wrap(errors), &ErrorAnalyzer.debug_error/1)
end
```

### 6. Performance Debugging

Identify performance bottlenecks in your workflows:

#### Performance Monitoring

```elixir
defmodule PerformanceMonitor do
  def setup_performance_monitoring(slow_threshold_ms \\ 100) do
    :telemetry.attach(
      "reactor-performance",
      [:reactor, :step, :run, :stop],
      &monitor_step_performance/4,
      %{threshold: slow_threshold_ms * 1_000_000}  # Convert to nanoseconds
    )
  end
  
  def monitor_step_performance(_event, measurements, metadata, config) do
    if measurements.duration > config.threshold do
      duration_ms = measurements.duration / 1_000_000
      
      IO.puts("🐌 Slow step detected:")
      IO.puts("   Step: #{metadata.step.name}")
      IO.puts("   Duration: #{duration_ms}ms")
      IO.puts("   Module: #{metadata.step.impl}")
    end
  end
end

PerformanceMonitor.setup_performance_monitoring(50)  # 50ms threshold
```

#### Concurrency Debugging

```elixir
defmodule ConcurrencyDebugger do
  def debug_concurrency(reactor_module, inputs) do
    # Monitor async step spawning
    :telemetry.attach(
      "concurrency-debug",
      [:reactor, :step, :process, :start],
      fn _event, _measurements, metadata, _config ->
        IO.puts("🚀 Spawned async step: #{metadata.step.name} (PID: #{inspect(metadata.pid)})")
      end,
      nil
    )
    
    # Run with limited concurrency for debugging
    Reactor.run(reactor_module, inputs, max_concurrency: 2)
  end
end
```

### 7. Interactive Debugging

Use IEx for interactive debugging sessions:

#### Adding Breakpoints

```elixir
defmodule MyApp.Steps.DebugStep do
  use Reactor.Step
  
  def run(arguments, context, _options) do
    IO.puts("🔍 Interactive debugging point")
    IO.puts("Arguments: #{inspect(arguments)}")
    IO.puts("Context: #{inspect(context)}")
    
    # Start interactive session
    require IEx; IEx.pry()
    
    {:ok, arguments}
  end
end

# Use in reactor
step :debug_point, MyApp.Steps.DebugStep do
  argument :data, result(:previous_step)
end
```

#### Runtime Inspection

```elixir
defmodule ReactorInspector do
  def inspect_reactor_state(reactor_module) do
    {:ok, reactor_struct} = Reactor.Info.to_struct(reactor_module)
    
    IO.puts("📊 Reactor Analysis:")
    IO.puts("   Inputs: #{length(reactor_struct.inputs)}")
    IO.puts("   Steps: #{length(reactor_struct.steps)}")
    
    IO.puts("\n📝 Step Details:")
    Enum.each(reactor_struct.steps, fn step ->
      IO.puts("   #{step.name}:")
      IO.puts("     Module: #{step.impl}")
      IO.puts("     Async: #{step.async?}")
      IO.puts("     Arguments: #{length(step.arguments)}")
    end)
    
    if reactor_struct.plan do
      IO.puts("\n🗺️ Execution Plan:")
      IO.inspect(reactor_struct.plan)
    end
  end
end
```

## Debugging Workflow

1. **Start with synchronous execution** (`async?: false`)
2. **Add debug steps** at key points in your workflow
3. **Generate visual diagrams** to understand data flow
4. **Set up telemetry monitoring** for performance insights
5. **Use error analysis** to understand failure modes
6. **Add interactive breakpoints** for complex issues

## Common Debugging Scenarios

### Step Not Executing

```elixir
# Check if dependencies are met
debug :before_problem_step do
  argument :data, result(:previous_step)
end

step :problem_step do
  argument :data, result(:previous_step)
  # ...
end
```

### Unexpected Results

```elixir
# Compare expected vs actual
debug :validate_result do
  argument :result, result(:calculation_step)
end

step :verify_result do
  argument :result, result(:calculation_step)
  run fn %{result: result}, _context ->
    expected = 42
    if result == expected do
      {:ok, result}
    else
      {:error, "Expected #{expected}, got #{result}"}
    end
  end
end
```

## Related Guides

- [Testing Strategies](testing-strategies.md) - Test your debugging assumptions
- [Performance Optimization](performance-optimization.md) - Optimize identified bottlenecks
- [Error Handling Tutorial](../tutorials/02-error-handling.md) - Handle errors gracefully

This comprehensive debugging approach helps you quickly identify and resolve issues in your reactor workflows.
