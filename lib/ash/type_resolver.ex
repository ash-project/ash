# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.TypeResolver do
  @moduledoc false
  use GenServer

  @timeout 30_000

  @doc """
  Ensure the TypeResolver is started. Idempotent.
  """
  def ensure_started do
    case Process.whereis(__MODULE__) do
      nil ->
        case GenServer.start_link(__MODULE__, %{}, name: __MODULE__) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
        end

      _pid ->
        :ok
    end
  end

  @doc false
  def register_known_field(resource, field_name, type, constraints) do
    GenServer.call(__MODULE__, {:register_known, resource, field_name, type, constraints})
  end

  @doc false
  def register_auto(resource, calc_name, calc, dsl_state, deps) do
    GenServer.call(__MODULE__, {:register_auto, resource, calc_name, calc, dsl_state, deps})
  end

  @doc false
  def done_registering(resource) do
    GenServer.call(__MODULE__, {:done_registering, resource})
  end

  @doc false
  def resolve(resource, calc_name, timeout \\ @timeout) do
    GenServer.call(__MODULE__, {:resolve, resource, calc_name}, timeout)
  catch
    :exit, {:timeout, _} ->
      {:error,
       "Timed out waiting for type resolution of `#{calc_name}` on `#{inspect(resource)}`. " <>
         "This may indicate a circular dependency between `:auto` calculations across resources."}
  end

  @doc false
  def shutdown do
    case Process.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.stop(__MODULE__, :normal)
    end
  catch
    :exit, _ -> :ok
  end

  @impl true
  def init(_) do
    {:ok,
     %{
       known_types: %{},
       pending: %{},
       waiters: [],
       registered: MapSet.new()
     }}
  end

  @impl true
  def handle_call({:register_known, resource, field_name, type, constraints}, _from, state) do
    state = put_in(state.known_types[{resource, field_name}], {type, constraints})
    state = run_resolution_pass(state)
    {:reply, :ok, state}
  end

  def handle_call(
        {:register_auto, resource, calc_name, calc, dsl_state, deps},
        _from,
        state
      ) do
    key = {resource, calc_name}

    pending_entry = %{
      calc: calc,
      dsl_state: dsl_state,
      deps: deps
    }

    state = put_in(state.pending[key], pending_entry)
    state = run_resolution_pass(state)
    {:reply, :ok, state}
  end

  def handle_call({:done_registering, resource}, _from, state) do
    state = %{state | registered: MapSet.put(state.registered, resource)}
    state = run_resolution_pass(state)
    {:reply, :ok, state}
  end

  def handle_call({:resolve, resource, calc_name}, from, state) do
    key = {resource, calc_name}

    case Map.fetch(state.known_types, key) do
      {:ok, {type, constraints}} ->
        {:reply, {:ok, type, constraints}, state}

      :error ->
        state = %{state | waiters: [{from, resource, calc_name} | state.waiters]}
        state = run_resolution_pass(state)
        {:noreply, state}
    end
  end

  defp run_resolution_pass(state) do
    {state, progress} = resolve_pending(state)

    if progress do
      run_resolution_pass(state)
    else
      check_deadlocks(state)
    end
  end

  defp resolve_pending(state) do
    Enum.reduce(state.pending, {state, false}, fn {{resource, calc_name} = key, entry},
                                                  {state, progress} ->
      unresolved = Enum.reject(entry.deps, &Map.has_key?(state.known_types, &1))

      if unresolved == [] do
        case Ash.TypeResolver.ExprAnalyzer.resolve_type(
               resource,
               entry.dsl_state,
               get_expression(entry.calc),
               state.known_types
             ) do
          {:ok, type, constraints} ->
            state = put_in(state.known_types[key], {type, constraints})
            state = %{state | pending: Map.delete(state.pending, key)}
            state = reply_to_waiters(state, resource, calc_name, {:ok, type, constraints})
            {state, true}

          {:deps, new_deps} ->
            state = put_in(state.pending[key], %{entry | deps: new_deps})
            {state, false}

          {:error, reason} ->
            state = %{state | pending: Map.delete(state.pending, key)}
            state = reply_to_waiters(state, resource, calc_name, {:error, reason})
            {state, true}
        end
      else
        {state, progress}
      end
    end)
  end

  defp reply_to_waiters(state, resource, calc_name, reply) do
    {matching, remaining} =
      Enum.split_with(state.waiters, fn {_from, r, c} -> r == resource and c == calc_name end)

    Enum.each(matching, fn {from, _, _} -> GenServer.reply(from, reply) end)
    %{state | waiters: remaining}
  end

  defp check_deadlocks(state) do
    waiting_keys =
      state.waiters
      |> Enum.map(fn {_from, resource, calc_name} -> {resource, calc_name} end)
      |> Enum.filter(&Map.has_key?(state.pending, &1))

    if waiting_keys == [] do
      state
    else
      case find_cycle(state.pending) do
        {:cycle, cycle} ->
          cycle_desc =
            Enum.map_join(cycle, " -> ", fn {resource, name} ->
              "#{inspect(resource)}.#{name}"
            end)

          error =
            {:error,
             "Circular dependency detected between `:auto` calculations: #{cycle_desc}. " <>
               "At least one calculation in the cycle must have an explicit type."}

          cycle_set = MapSet.new(cycle)

          {cycle_waiters, remaining} =
            Enum.split_with(state.waiters, fn {_from, r, c} ->
              MapSet.member?(cycle_set, {r, c})
            end)

          Enum.each(cycle_waiters, fn {from, _, _} -> GenServer.reply(from, error) end)

          state = %{
            state
            | waiters: remaining,
              pending: Map.drop(state.pending, cycle)
          }

          state

        :no_cycle ->
          state
      end
    end
  end

  defp find_cycle(pending) do
    graph = Map.new(pending, fn {key, entry} -> {key, entry.deps} end)
    nodes = Map.keys(graph)

    Enum.reduce_while(nodes, :no_cycle, fn node, :no_cycle ->
      case dfs_cycle(node, graph, [], MapSet.new()) do
        {:cycle, cycle} -> {:halt, {:cycle, cycle}}
        :no_cycle -> {:cont, :no_cycle}
      end
    end)
  end

  defp dfs_cycle(node, graph, path, visited) do
    if node in path do
      cycle_start = Enum.find_index(path, &(&1 == node))
      cycle = Enum.slice(path, cycle_start..-1//1) ++ [node]
      {:cycle, cycle}
    else
      if MapSet.member?(visited, node) do
        :no_cycle
      else
        deps = Map.get(graph, node, [])
        pending_deps = Enum.filter(deps, &Map.has_key?(graph, &1))

        Enum.reduce_while(pending_deps, :no_cycle, fn dep, :no_cycle ->
          case dfs_cycle(dep, graph, path ++ [node], MapSet.put(visited, node)) do
            {:cycle, _} = result -> {:halt, result}
            :no_cycle -> {:cont, :no_cycle}
          end
        end)
      end
    end
  end

  defp get_expression(%{calculation: {Ash.Resource.Calculation.Expression, opts}}) do
    opts[:expr]
  end
end
