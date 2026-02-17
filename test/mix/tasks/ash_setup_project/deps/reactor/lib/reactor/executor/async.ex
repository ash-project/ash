# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.Async do
  @moduledoc """
  Handle the asynchronous execution of a batch of steps, along with any
  mutations to the reactor or execution state.
  """
  alias Reactor.{Error.Invalid.RetriesExceededError, Executor, Executor.ConcurrencyTracker, Step}
  require Logger

  @doc """
  Start as many of the provided steps as possible.

  Takes into account he maximum concurrency and available work slots.
  """
  @spec start_steps(Reactor.t(), Executor.State.t(), [Step.t()], Supervisor.supervisor()) ::
          {:continue | :recurse, Reactor.t(), Executor.State.t()} | {:error, any}
  def start_steps(
        reactor,
        state,
        steps,
        supervisor \\ {:via, PartitionSupervisor, {Reactor.TaskSupervisor, self()}}
      )

  def start_steps(reactor, state, [], _supervisor), do: {:continue, reactor, state}

  def start_steps(reactor, state, steps, supervisor) do
    available_steps = length(steps)

    locked_concurrency =
      acquire_concurrency_resource_from_pool(state.concurrency_key, available_steps)

    process_contexts = Executor.Hooks.get_process_contexts(reactor)

    started =
      steps
      |> Enum.take(locked_concurrency)
      |> Enum.reduce_while(%{}, fn step, started ->
        case start_task_for_step(
               reactor,
               state,
               step,
               supervisor,
               state.concurrency_key,
               process_contexts
             ) do
          {:ok, task} -> {:cont, Map.put(started, task, step)}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    if map_size(started) > 0 do
      reactor = add_task_edges(reactor, started)
      state = %{state | current_tasks: Map.merge(state.current_tasks, started)}
      {:recurse, reactor, state}
    else
      {:continue, reactor, state}
    end
  end

  defp start_task_for_step(reactor, state, step, supervisor, pool_key, process_contexts) do
    {:ok,
     Task.Supervisor.async_nolink(
       supervisor,
       Executor.StepRunner,
       :run_async,
       [reactor, state, step, pool_key, process_contexts]
     )}
  rescue
    error -> {:error, error}
  end

  @doc """
  Handle zero or one completed async steps and then decide what to do.
  """
  @spec handle_completed_steps(Reactor.t(), Executor.State.t()) ::
          {:recurse | :continue | :undo | :halt, Reactor.t(), Executor.State.t()}
  def handle_completed_steps(reactor, state) do
    completed_task_results = get_normalised_task_results(state, timeout: 100)
    handle_completed_task_results(reactor, state, completed_task_results)
  end

  defp handle_completed_task_results(reactor, state, []),
    do: {:continue, reactor, state}

  defp handle_completed_task_results(reactor, state, completed_task_results) do
    Enum.reduce(
      completed_task_results,
      {:recurse, reactor, state},
      fn task_result, {status, reactor, state} ->
        {new_status, reactor, state} = handle_completed_step(reactor, state, task_result)

        if got_worse?(status, new_status) do
          {new_status, reactor, state}
        else
          {status, reactor, state}
        end
      end
    )
  end

  defp got_worse?(:recurse, :undo), do: true
  defp got_worse?(:recurse, :halt), do: true
  defp got_worse?(:undo, :halt), do: true
  defp got_worse?(_old, _new), do: false

  defp handle_completed_step(reactor, state, {task, step, {:skip, result}}) do
    state = %{state | skipped: MapSet.put(state.skipped, step.ref)}
    handle_completed_step(reactor, state, {task, step, result})
  end

  defp handle_completed_step(reactor, state, {task, step, {:backoff, delay, result}}) do
    backoff = Executor.Backoff.delay(delay)

    plan =
      reactor.plan
      |> Graph.add_vertex(backoff)
      |> Graph.add_edge(backoff, step, label: :backoff)

    reactor = %{reactor | plan: plan}
    handle_completed_step(reactor, state, {task, step, result})
  end

  defp handle_completed_step(reactor, state, {task, step, {:ok, value, new_steps}}) do
    state =
      state
      |> drop_task(task)

    reactor =
      reactor
      |> drop_from_plan(task)
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
          |> append_steps(recursive_steps)
          |> append_steps(new_steps)
      end

    {:recurse, reactor, state}
  end

  defp handle_completed_step(reactor, state, {task, step, {:retry, error}}) do
    state =
      state
      |> increment_retries(step)
      |> drop_task(task)

    reactor =
      reactor
      |> drop_from_plan(task)

    if Map.get(state.retries, step.ref) > step.max_retries do
      error =
        error ||
          RetriesExceededError.exception(
            step: step,
            retry_count: Map.get(state.retries, step.ref)
          )

      reactor = drop_from_plan(reactor, step)
      {:undo, reactor, add_error(state, error)}
    else
      {:recurse, reactor, state}
    end
  end

  defp handle_completed_step(reactor, state, {task, step, {:error, error}}) do
    state =
      state
      |> drop_task(task)
      |> add_error(error)

    reactor =
      reactor
      |> drop_from_plan(task)
      |> drop_from_plan(step)

    {:undo, reactor, state}
  end

  defp handle_completed_step(reactor, state, {task, step, {:halt, value}}) do
    state =
      state
      |> drop_task(task)

    reactor =
      reactor
      |> drop_from_plan(task)
      |> drop_from_plan(step)
      |> store_intermediate_result(step, value)

    {:halt, reactor, state}
  end

  defp get_normalised_task_results(%{current_tasks: current_tasks}, opts) do
    current_tasks
    |> Map.keys()
    |> Task.yield_many(opts)
    |> Stream.reject(&is_nil(elem(&1, 1)))
    |> Stream.map(fn
      {task, {:ok, result}} ->
        {task, normalise_result(result)}

      {task, {:exit, reason}} ->
        {task, normalise_result({:error, reason})}
    end)
    |> Enum.map(fn {task, result} ->
      {task, Map.fetch!(current_tasks, task), result}
    end)
  end

  defp normalise_result({:error, reason}), do: {:error, reason}
  defp normalise_result({:halt, reason}), do: {:halt, reason}
  defp normalise_result(:retry), do: {:retry, nil}
  defp normalise_result({:retry, reason}), do: {:retry, reason}
  defp normalise_result({:ok, value}), do: {:ok, value, []}
  defp normalise_result({:ok, value, steps}) when is_list(steps), do: {:ok, value, steps}
  defp normalise_result({:skip, result}), do: {:skip, normalise_result(result)}

  defp normalise_result({:backoff, delay, result}),
    do: {:backoff, delay, normalise_result(result)}

  defp drop_task(state, task) do
    ConcurrencyTracker.release(state.concurrency_key, 1)

    %{state | current_tasks: Map.delete(state.current_tasks, task)}
  end

  defp increment_retries(state, step) do
    %{state | retries: Map.update(state.retries, step.ref, 1, &(&1 + 1))}
  end

  defp drop_from_plan(reactor, step) do
    %{reactor | plan: Graph.delete_vertex(reactor.plan, step)}
  end

  defp add_error(state, error) do
    %{state | errors: [error | state.errors]}
  end

  defp store_intermediate_result(reactor, step, value) do
    %{reactor | intermediate_results: Map.put(reactor.intermediate_results, step.name, value)}
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

  defp store_successful_results_in_the_undo_stack(reactor, completed_step_results)
       when map_size(completed_step_results) == 0,
       do: reactor

  defp store_successful_results_in_the_undo_stack(reactor, completed_step_results) do
    undoable_successful_results =
      completed_step_results
      |> Enum.filter(fn
        {step, {:ok, _, _}} -> Step.can?(step, :undo)
        {step, {:halt, _}} -> Step.can?(step, :undo)
        _ -> false
      end)
      |> Map.new(fn
        {step, {:ok, value, _}} -> {step, value}
        {step, {:halt, value}} -> {step, value}
      end)

    %{reactor | undo: Enum.concat(reactor.undo, undoable_successful_results)}
  end

  defp store_intermediate_results(reactor, completed_step_results)
       when map_size(completed_step_results) == 0,
       do: reactor

  defp store_intermediate_results(reactor, completed_step_results) do
    intermediate_results =
      completed_step_results
      |> Enum.filter(fn
        {step, {:ok, _, []}} ->
          Graph.out_degree(reactor.plan, step) > 0 || reactor.return == step.name

        {_step, {:ok, _, _}} ->
          true

        {_step, {:halt, _}} ->
          true

        _ ->
          false
      end)
      |> Map.new(fn
        {step, {:ok, value, _}} -> {step.name, value}
        {step, {:halt, value}} -> {step.name, value}
      end)

    %{
      reactor
      | intermediate_results: Map.merge(reactor.intermediate_results, intermediate_results)
    }
  end

  @doc """
  When the Reactor needs to shut down for any reason, we need to await all the
  currently running asynchronous steps and delete any task vertices.
  """
  @spec collect_remaining_tasks_for_shutdown(Reactor.t(), Executor.State.t()) ::
          {Reactor.t(), Executor.State.t()}
  def collect_remaining_tasks_for_shutdown(reactor, state)
      when map_size(state.current_tasks) == 0 do
    {delete_all_task_vertices(reactor), state}
  end

  def collect_remaining_tasks_for_shutdown(reactor, state) do
    remaining_task_results =
      get_normalised_task_results(state, timeout: state.halt_timeout, on_timeout: :ignore)

    release_concurrency_resources_to_pool(state.concurrency_key, length(remaining_task_results))

    remaining_step_results =
      remaining_task_results
      |> Map.new(fn {_task, step, result} -> {step, result} end)

    finished_tasks = remaining_step_results |> Enum.map(&elem(&1, 0))

    reactor =
      reactor
      |> store_successful_results_in_the_undo_stack(remaining_step_results)
      |> store_intermediate_results(remaining_step_results)

    unfinished_tasks =
      state.current_tasks
      |> Map.delete(finished_tasks)

    unfinished_task_count = map_size(unfinished_tasks)

    if unfinished_task_count > 0 do
      Logger.warning(fn ->
        unfinished_steps =
          unfinished_tasks
          |> Map.values()
          |> Enum.map_join("\n  * ", &inspect/1)

        """
        Waited #{state.halt_timeout}ms for async steps to complete, however #{unfinished_task_count} are still running, will be abandoned and cannot be undone.

          * #{unfinished_steps}
        """
      end)

      unfinished_tasks
      |> Map.keys()
      |> Enum.each(&Task.ignore/1)
    end

    {delete_all_task_vertices(reactor), %{state | current_tasks: %{}}}
  end

  defp add_task_edges(reactor, started_tasks) do
    plan =
      Enum.reduce(started_tasks, reactor.plan, fn {task, step}, plan ->
        Graph.add_edge(plan, task, step, label: :executing)
      end)

    %{reactor | plan: plan}
  end

  defp delete_vertices(reactor, []), do: reactor

  defp delete_vertices(reactor, completed_tasks),
    do: %{reactor | plan: Graph.delete_vertices(reactor.plan, completed_tasks)}

  defp delete_all_task_vertices(reactor) do
    task_vertices =
      reactor.plan
      |> Graph.vertices()
      |> Enum.filter(&is_struct(&1, Task))

    delete_vertices(reactor, task_vertices)
  end

  defp append_steps(reactor, steps) do
    %{reactor | steps: Enum.concat(steps, reactor.steps)}
  end

  defp release_concurrency_resources_to_pool(pool_key, how_many) do
    ConcurrencyTracker.release(pool_key, how_many)
  end

  defp acquire_concurrency_resource_from_pool(pool_key, requested) do
    {:ok, actual} = ConcurrencyTracker.acquire(pool_key, requested)
    actual
  end
end
