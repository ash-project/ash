# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.Sync do
  @moduledoc """
  Handle the synchronous execution of a single step, along with any mutations to
  the reactor or execution state.
  """

  alias Reactor.Error.Invalid.RetriesExceededError, as: RetriesExceededError
  alias Reactor.{Executor, Step}

  @doc """
  Try and run a step synchronously.
  """
  @spec run(Reactor.t(), Executor.State.t(), Step.t() | nil) ::
          {:continue | :recurse | :halt | :undo, Reactor.t(), Executor.State.t()}
  def run(reactor, state, nil), do: {:continue, reactor, state}

  def run(reactor, state, step) do
    case Executor.StepRunner.run(reactor, state, step, state.concurrency_key) do
      {:skip, result} ->
        state = %{state | skipped: MapSet.put(state.skipped, step.ref)}
        handle_completed_step(reactor, state, step, result)

      result ->
        handle_completed_step(reactor, state, step, result)
    end
  end

  defp handle_completed_step(reactor, state, step, {:backoff, delay, result}) do
    backoff = Executor.Backoff.delay(delay)

    plan =
      reactor.plan
      |> Graph.add_vertex(backoff)
      |> Graph.add_edge(backoff, step, label: :backoff)

    reactor = %{reactor | plan: plan}
    handle_completed_step(reactor, state, step, result)
  end

  defp handle_completed_step(reactor, state, step, :retry) do
    handle_completed_step(reactor, state, step, {:retry, nil})
  end

  defp handle_completed_step(reactor, state, step, {:retry, error}) do
    state = increment_retries(state, step)

    if Map.get(state.retries, step.ref) > step.max_retries do
      reactor = drop_from_plan(reactor, step)

      error =
        error ||
          RetriesExceededError.exception(
            step: step,
            retry_count: Map.get(state.retries, step.ref)
          )

      {:undo, reactor, %{state | errors: [error | state.errors]}}
    else
      {:recurse, reactor, state}
    end
  end

  defp handle_completed_step(reactor, state, step, {:ok, value, new_steps}) do
    reactor =
      reactor
      |> maybe_store_undo(step, value, state)
      |> maybe_store_intermediate_result(step, value)

    reactor =
      case Enum.split_with(new_steps, &(&1.name == step.name)) do
        {[], new_steps} ->
          reactor
          |> drop_from_plan(step)
          |> append_steps(new_steps)

        {recursive_steps, new_steps} ->
          recursive_steps = Enum.map(recursive_steps, &%{&1 | ref: step.ref})

          reactor
          |> store_intermediate_result(step, value)
          |> append_steps(recursive_steps)
          |> append_steps(new_steps)
      end

    {:recurse, reactor, state}
  end

  defp handle_completed_step(reactor, state, step, {:error, reason}) do
    state = %{state | errors: [reason | state.errors]}
    reactor = drop_from_plan(reactor, step)
    {:undo, reactor, state}
  end

  defp handle_completed_step(reactor, state, step, {:halt, value}) do
    reactor =
      reactor
      |> drop_from_plan(step)
      |> store_intermediate_result(step, value)

    {:halt, reactor, state}
  end

  defp increment_retries(state, step) do
    %{state | retries: Map.update(state.retries, step.ref, 1, &(&1 + 1))}
  end

  defp drop_from_plan(reactor, step) do
    %{reactor | plan: Graph.delete_vertex(reactor.plan, step)}
  end

  defp maybe_store_undo(reactor, step, value, state) do
    cond do
      MapSet.member?(state.skipped, step.ref) -> reactor
      Step.can?(step, :undo) -> %{reactor | undo: [{step, value} | reactor.undo]}
      true -> reactor
    end
  end

  defp maybe_store_intermediate_result(reactor, step, value) when reactor.return == step.name do
    store_intermediate_result(reactor, step, value)
  end

  defp maybe_store_intermediate_result(reactor, step, value) do
    if Graph.out_degree(reactor.plan, step) > 0 do
      store_intermediate_result(reactor, step, value)
    else
      reactor
    end
  end

  defp store_intermediate_result(reactor, step, value),
    do: %{reactor | intermediate_results: Map.put(reactor.intermediate_results, step.name, value)}

  defp append_steps(reactor, steps), do: %{reactor | steps: Enum.concat(steps, reactor.steps)}
end
