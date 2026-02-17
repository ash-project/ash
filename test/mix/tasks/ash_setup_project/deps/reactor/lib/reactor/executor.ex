# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor do
  @moduledoc """
  The Reactor executor.

  The executor handles the main loop of running a Reactor.

  The algorithm is somewhat confusing, so here it is in pseudocode:

  1. Find any async tasks (from a previous loop) which are completed. Either
     recurse or continue if none are found.
  2. Find any async steps in the plan which are ready to run (they have no
     in-edges in the graph) and start as many as possible (given the constraints
     of `max_concurrency` and the state of the concurrency pool).  Either start
     over, or continue if none are found.
  3. Find a single synchronous step which is ready to run and execute it. If
     there was one then recurse, otherwise continue.
  4. Check if there are no more steps left in the plan (there are zero
     vertices).  If so, collect the return value and exit, otherwise recurse.

  Whenever a step is run, whether run synchronously or asynchronously, the
  following happens:

  1. When the step is successful:
    a. If the step is undoable (ie `Step.can?(module, :undo)?` returns `true`)
       then the step and the result are stored in the Reactor's undo stack.
    b. If the result is depended upon by another step (the graph has out-edges
       for the step) _or_ the step is asking the reactor to halt then the
       result is stored in the Reactor's intermediate results.
    c. The step is removed from the graph (along with it's out-edges, freeing
       up it's dependents to run).
  2. When the step is unsuccessful (returns an error tuple or raises):
    a. If the step can be compensated then compensation is attempted up to five
       times before giving up.
    b. The reactor iterates it's undo stack calling undo on each step.
  3. When a step or compensation asks for a retry then the step is placed back
     in the graph to be run again next iteration.
  """
  alias Reactor.{
    Error.Internal.MissingReturnResultError,
    Error.Validation.MissingReturnError,
    Error.Validation.StateError,
    Executor,
    Executor.Backoff,
    Executor.ConcurrencyTracker,
    Planner,
    Step
  }

  @doc """
  Run a reactor.

  Provided a Reactor which has been planned and the correct inputs, then run
  the Reactor until completion, halting or failure.

  You probably shouldn't call this directly, but use `Reactor.run/4` instead.
  """
  @spec run(Reactor.t(), Reactor.inputs(), Reactor.context(), Reactor.run_options()) ::
          {:ok, any} | {:ok, any, Reactor.t()} | {:halted, Reactor.t()} | {:error, any}
  def run(reactor, inputs \\ %{}, context \\ %{}, options \\ [])

  def run(reactor, _inputs, _context, _options) when is_nil(reactor.return),
    do: {:error, MissingReturnError.exception(reactor: reactor)}

  def run(reactor, inputs, context, options) when reactor.state in ~w[pending halted]a do
    with {:ok, reactor, state} <- Executor.Init.init(reactor, inputs, context, options),
         {:ok, context} <- Executor.Hooks.init(reactor, reactor.context) do
      execute(%{reactor | context: context}, state)
    end
  end

  def run(reactor, _inputs, _context, _options),
    do:
      {:error,
       StateError.exception(
         reactor: reactor,
         state: reactor.state,
         expected: ~w[pending halted]a
       )}

  @doc """
  Undo a previously successful Reactor.
  """
  @spec undo(Reactor.t(), Reactor.context(), Reactor.undo_options()) :: :ok | {:error, any}
  def undo(reactor, context, options) do
    inputs =
      reactor.context
      |> Map.get(:private, %{})
      |> Map.get(:inputs, %{})

    with {:ok, reactor, state} <- Executor.Init.init(reactor, inputs, context, options) do
      handle_undo(reactor, state)
    end
  end

  defp execute(reactor, state) when state.max_iterations == 0 do
    {reactor, _status} = Executor.Async.collect_remaining_tasks_for_shutdown(reactor, state)
    maybe_release_pool(state)
    {:halted, %{reactor | state: :halted}}
  end

  defp execute(reactor, state) do
    with {:continue, reactor, state} <- maybe_timeout(reactor, state),
         {:continue, reactor, state} <- handle_unplanned_steps(reactor, state),
         {:continue, reactor, state} <- handle_completed_async_steps(reactor, state),
         {:continue, ready_steps} <- find_ready_steps(reactor, state),
         {:continue, reactor, state} <- start_ready_async_steps(reactor, state, ready_steps),
         {:continue, reactor, state} <- run_ready_sync_step(reactor, state, ready_steps),
         {:continue, reactor, state} <- maybe_run_any_step_sync(reactor, state, ready_steps),
         {:continue, reactor, state} <- prune_expired_backoffs(reactor, state),
         {:continue, reactor} <- all_done(reactor) do
      execute(reactor, subtract_iteration(state))
    else
      {:recurse, reactor, state} ->
        execute(reactor, subtract_iteration(state))

      {:undo, reactor, state} ->
        handle_undo(reactor, state)

      {:halt, reactor, _state} ->
        maybe_release_pool(state)

        case Executor.Hooks.halt(reactor, reactor.context) do
          {:ok, context} -> {:halted, %{reactor | context: context, state: :halted}}
          {:error, reason} -> {:error, reason}
        end

      {:ok, result, reactor} ->
        maybe_release_pool(state)

        with {:ok, result} <- Executor.Hooks.complete(reactor, result, reactor.context) do
          if state.fully_reversible? do
            {:ok, result, reactor}
          else
            {:ok, result}
          end
        end

      {:error, reason} ->
        maybe_release_pool(state)

        Executor.Hooks.error(reactor, reason, reactor.context)
    end
  end

  defp maybe_timeout(reactor, state) when state.timeout == :infinity,
    do: {:continue, reactor, state}

  defp maybe_timeout(reactor, state) do
    if DateTime.diff(DateTime.utc_now(), state.started_at, :millisecond) >= state.timeout do
      {reactor, _status} = Executor.Async.collect_remaining_tasks_for_shutdown(reactor, state)
      {:halt, reactor, state}
    else
      {:continue, reactor, state}
    end
  end

  defp handle_unplanned_steps(reactor, state) when reactor.steps == [],
    do: {:continue, reactor, state}

  defp handle_unplanned_steps(reactor, state) do
    case Planner.plan(reactor) do
      {:ok, reactor} -> {:recurse, reactor, state}
      {:error, reason} -> {:undo, reactor, %{state | errors: [reason | state.errors]}}
    end
  end

  defp handle_completed_async_steps(reactor, state) when state.async? == false,
    do: {:continue, reactor, state}

  defp handle_completed_async_steps(reactor, state),
    do: Executor.Async.handle_completed_steps(reactor, state)

  defp start_ready_async_steps(reactor, state, _) when state.async? == false,
    do: {:continue, reactor, state}

  defp start_ready_async_steps(reactor, state, []), do: {:continue, reactor, state}

  defp start_ready_async_steps(reactor, state, steps) do
    steps = Enum.filter(steps, &Step.async?/1)

    Executor.Async.start_steps(reactor, state, steps)
  end

  defp run_ready_sync_step(reactor, state, []), do: {:continue, reactor, state}

  defp run_ready_sync_step(reactor, state, [step | _]) when state.async? == false,
    do: Executor.Sync.run(reactor, state, step)

  defp run_ready_sync_step(reactor, state, steps) do
    steps
    |> Enum.find(&(!Step.async?(&1)))
    |> case do
      nil ->
        {:continue, reactor, state}

      step ->
        Executor.Sync.run(reactor, state, step)
    end
  end

  # This seems a little unintuitive, but this is what allows reactors who are
  # sharing a concurrency pool to move forward even then there's no concurrency
  # left without deadlocking.
  #
  # It's a complicated scenario, so let's lay out the pieces:
  #
  # 1. When a new reactor is started it allocates a concurrency pool using
  #    `Reactor.Executor.ConcurrencyTracker` **unless** it is explicitly passed
  #    a `concurrency_key` option.
  # 2. Every time a reactor runs an async step it starts a `Task` and consumes a
  #    space in the concurrency pool (if possible).
  # 3. Every task that is started has it's concurrency key stored in it's
  #    process dictionary (actually a stack of them because we may be multiple
  #    nested reactors deep).
  # 4. If that async step then turns around and runs a new reactor with shared
  #    concurrency then that reactor is already consuming a concurrency slot and
  #    may not be able to allocate any more slots for it's tasks.
  #
  # This situation can lead to a deadlock where we have multiple reactors all in
  # a tight loop trying to start tasks but none of them able to proceed.
  #
  # We detect this situation by:
  #
  # 1. We are unable to start any async steps (`start_ready_async_steps/3`
  #    returns `:continue`).
  # 2. We are unable to start any sync steps (`run_ready_sync_step/3` returns
  #    `:continue`).
  # 3. We have any steps which can be run (ie async ones which we couldn't
  #    start).
  # 4. Our concurrency key is in the process dictionary.
  #
  # If all four of these conditions are met we pick the first step and run it
  # synchronously.  This is fine because the reactor process itself is a task in
  # another reactor so in effect is still running asynchronously.
  defp maybe_run_any_step_sync(reactor, state, []), do: {:continue, reactor, state}

  defp maybe_run_any_step_sync(reactor, state, [step | _]) do
    :__reactor__
    |> Process.get([])
    |> Enum.any?(&(&1.concurrency_key == state.concurrency_key))
    |> if do
      Executor.Sync.run(reactor, state, step)
    else
      {:continue, reactor, state}
    end
  end

  defp subtract_iteration(state) when state.max_iterations == :infinity, do: state

  defp subtract_iteration(state) when state.max_iterations > 0,
    do: %{state | max_iterations: state.max_iterations - 1}

  defp handle_undo(reactor, state) do
    handle_undo(%{reactor | state: :failed, undo: []}, state, Enum.reverse(reactor.undo))
  end

  defp handle_undo(_reactor, state, []) when state.errors == [], do: :ok

  defp handle_undo(reactor, state, []) do
    error = Reactor.Error.to_class(state.errors)
    Executor.Hooks.error(reactor, error, reactor.context)
  end

  defp handle_undo(reactor, state, [{step, value} | tail]) do
    case Executor.StepRunner.undo(reactor, state, step, value, state.concurrency_key) do
      :ok -> handle_undo(reactor, state, tail)
      {:error, reason} -> handle_undo(reactor, %{state | errors: [reason | state.errors]}, tail)
    end
  end

  defp all_done(reactor) do
    with 0 <- Graph.num_vertices(reactor.plan),
         {:ok, value} <- Map.fetch(reactor.intermediate_results, reactor.return) do
      {:ok, value, %{reactor | state: :successful}}
    else
      :error ->
        {:error, MissingReturnResultError.exception(reactor: reactor)}

      n when is_integer(n) ->
        {:continue, reactor}
    end
  end

  defp find_ready_steps(reactor, state) when state.max_concurrency > 0 do
    steps =
      reactor.plan
      |> Graph.vertices()
      |> Stream.filter(fn
        step when is_struct(step, Step) -> Graph.in_degree(reactor.plan, step) == 0
        _ -> false
      end)
      |> Enum.take(state.max_concurrency)

    {:continue, steps}
  end

  defp find_ready_steps(reactor, _state) do
    reactor.plan
    |> Graph.vertices()
    |> Enum.find(fn
      step when is_struct(step, Step) -> Graph.in_degree(reactor.plan, step) == 0
      _ -> false
    end)
    |> case do
      nil -> {:continue, []}
      step -> {:continue, [step]}
    end
  end

  defp prune_expired_backoffs(reactor, state) do
    now = System.monotonic_time(:millisecond)

    reactor.plan
    |> Graph.vertices()
    |> Enum.filter(&(is_struct(&1, Backoff) && &1.expires_at <= now))
    |> case do
      [] ->
        {:continue, reactor, state}

      to_remove ->
        {:recurse, %{reactor | plan: Graph.delete_vertices(reactor.plan, to_remove)}, state}
    end
  end

  defp maybe_release_pool(state) when state.pool_owner == true do
    ConcurrencyTracker.release_pool(state.concurrency_key)
  end

  defp maybe_release_pool(_), do: :ok
end
