<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Building Iterative Workflows with Recursive Execution

In this tutorial, you'll learn how to build iterative workflows that process data repeatedly until a condition is met. This is perfect for mathematical algorithms, data processing pipelines, and convergence calculations.

## What you'll build

Simple iterative algorithms demonstrating recursive patterns:
1. **Countdown Calculator** - Count down from a number to zero
2. **List Processor** - Process items one at a time until done

## You'll learn

- How Reactor's recursive execution works
- Setting up proper exit conditions and iteration limits
- Managing state between iterations
- When to use recursion vs other Reactor patterns
- Building convergent algorithms and iterative processors

## Prerequisites

- Complete the [Getting Started tutorial](01-getting-started.md)
- Complete the [Error Handling tutorial](02-error-handling.md)
- Complete the [Async Workflows tutorial](03-async-workflows.md)
- Complete the [Composition tutorial](04-composition.md)

## Step 1: Set up the project

If you don't have a project from the previous tutorials:

```bash
mix igniter.new reactor_tutorial --install reactor
cd reactor_tutorial
```

## Step 2: Understanding recursive execution

Reactor's recursive execution allows you to:

**Repeat until a condition is met:**
```elixir
recurse :calculate, CalculatorReactor do
  argument :value, input(:start_value)
  exit_condition fn %{done: done} -> done == true end
  max_iterations 100
end
```

**Process data iteratively:**
```elixir
recurse :process, ProcessorReactor do
  argument :remaining, input(:data_list)
  exit_condition fn %{remaining: list} -> Enum.empty?(list) end
  max_iterations 1000
end
```

## Step 3: Build a simple countdown reactor

Let's start with a simple recursive algorithm. Create `lib/countdown_reactor.ex`:

```elixir
defmodule CountdownReactor do
  use Reactor

  input :current_number

  step :countdown_step do
    argument :current_number, input(:current_number)

    run fn %{current_number: num}, _context ->
      new_number = num - 1
      {:ok, %{current_number: new_number}}
    end
  end

  return :countdown_step
end

defmodule CountdownExample do
  use Reactor

  input :start_number

  recurse :countdown, CountdownReactor do
    argument :current_number, input(:start_number)
    max_iterations 100
    exit_condition fn %{current_number: num} -> num <= 0 end
  end

  step :show_result do
    argument :final_state, result(:countdown)
    argument :start_number, input(:start_number)

    run fn %{final_state: state, start_number: start}, _context ->
      result = %{
        started_at: start,
        finished_at: state.current_number,
        message: "Counted down from #{start} to #{state.current_number}"
      }
      {:ok, result}
    end
  end

  return :show_result
end
```

## Step 4: Build an accumulator

Create `lib/accumulator.ex`:

```elixir
defmodule AccumulatorReactor do
  use Reactor

  input :current_total
  input :target_total

  step :add_random_amount do
    argument :current_total, input(:current_total)
    argument :target_total, input(:target_total)

    run fn %{current_total: current, target_total: target}, _context ->
      # Add a random amount between 1 and 10
      addition = :rand.uniform(10)
      new_total = current + addition
      
      reached_target = new_total >= target

      {:ok, %{
        current_total: new_total,
        target_total: target,
        last_addition: addition,
        reached_target: reached_target
      }}
    end
  end

  return :add_random_amount
end

defmodule AccumulatorExample do
  use Reactor

  input :target_score

  recurse :accumulate, AccumulatorReactor do
    argument :current_total, value(0)
    argument :target_total, input(:target_score)
    max_iterations 50
    exit_condition fn %{reached_target: reached} -> reached == true end
  end

  step :show_results do
    argument :final_state, result(:accumulate)
    argument :target, input(:target_score)

    run fn %{final_state: state, target: target}, _context ->
      result = %{
        target_score: target,
        final_total: state.current_total,
        last_addition: state.last_addition,
        message: "Reached #{state.current_total} (target was #{target})"
      }
      {:ok, result}
    end
  end

  return :show_results
end
```

## Step 5: Advanced example - Convergence algorithm

Now let's look at a more sophisticated pattern - iterative convergence. Create `lib/square_root_calculator.ex`:

```elixir
defmodule NewtonMethodReactor do
  use Reactor

  input :current_guess
  input :target_number

  step :newton_iteration do
    argument :current_guess, input(:current_guess)
    argument :target_number, input(:target_number)

    run fn %{current_guess: guess, target_number: target}, _context ->
      # Newton's method: x_new = (x + target/x) / 2
      new_guess = (guess + target / guess) / 2
      difference = abs(new_guess - guess)
      
      # Consider converged when difference is very small
      converged = difference < 0.0001

      {:ok, %{
        current_guess: new_guess,
        target_number: target,
        converged: converged
      }}
    end
  end

  return :newton_iteration
end

defmodule SquareRootCalculator do
  use Reactor

  input :number

  recurse :converge, NewtonMethodReactor do
    argument :current_guess, input(:number)  # Start with the number itself
    argument :target_number, input(:number)
    max_iterations 20
    exit_condition fn %{converged: converged} -> converged == true end
  end

  step :show_result do
    argument :final_state, result(:converge)
    argument :original_number, input(:number)

    run fn %{final_state: state, original_number: num}, _context ->
      result = %{
        number: num,
        square_root: state.current_guess,
        message: "√#{num} ≈ #{Float.round(state.current_guess, 6)}"
      }
      {:ok, result}
    end
  end

  return :show_result
end
```

## Step 6: Test the examples

Let's test our recursive reactors:

```bash
iex -S mix
```

```elixir
# Test the countdown example
{:ok, result} = Reactor.run(CountdownExample, %{start_number: 5})
IO.inspect(result.message)
# Should output: "Counted down from 5 to 0"

# Test the accumulator
{:ok, result} = Reactor.run(AccumulatorExample, %{target_score: 50})
IO.inspect(result.message)
# Should output something like: "Reached 53 (target was 50)"

# Test the square root calculator
{:ok, result} = Reactor.run(SquareRootCalculator, %{number: 25})
IO.inspect(result.message)
# Should output: "√25 ≈ 5.0"
```


## What you learned

You now understand Reactor's recursive execution:

- **Always provide termination conditions** - Use both `exit_condition` and `max_iterations`
- **Keep state simple** - Use flat maps with simple values
- **Test with small examples** - Start simple before building complexity
- **Convergence detection** - Check when iterative improvements become negligible

### When to use recursion:

**Use recursion for:**
- Counting and decrementing patterns
- Accumulation until a threshold is reached
- Iterative calculations with clear stopping conditions
- Mathematical convergence algorithms

**Avoid recursion for:**
- Simple sequential processing (use regular steps)
- One-time transformations (use map steps)
- Parallel processing of collections (use map steps with batching)

## What's next

You've mastered all core Reactor patterns! Ready for specialized guides:

- **[Testing Strategies](documentation/how-to/testing-strategies.md)** - Comprehensive testing approaches
- **[Performance Optimization](documentation/how-to/performance-optimization.md)** - Advanced performance techniques  
- **[Debugging Workflows](documentation/how-to/debugging-workflows.md)** - Troubleshooting complex reactors

## Common issues

**Recursion never terminates**: Ensure exit conditions can actually be met; always include `max_iterations` as backup

**"Maximum iterations exceeded" error**: Increase `max_iterations` or fix logic preventing proper convergence

**State not passing correctly**: Ensure output structure exactly matches input requirements; keep state flat and simple

**Exit condition never triggers**: Add debug logging to verify exit condition logic; check that condition field exists in state

Happy building iterative workflows! 🔄
