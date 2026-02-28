defmodule Ash.TypeResolver do
  @moduledoc false
  use GenServer

  # A locally-registered GenServer that coordinates type resolution for `:auto`
  # calculations across all compiling resources.
  #
  # When multiple resources are compiled in parallel and they reference each other's
  # `:auto` calculations, no single resource can resolve in isolation. This GenServer
  # acts as a central coordinator:
  #
  # 1. Each resource registers its known field types (attributes, typed calcs, aggregates)
  # 2. Each resource registers its `:auto` calculations with their expressions and deps
  # 3. After each registration, a resolution pass runs to resolve anything newly unblocked
  # 4. Resources call `resolve/3` which blocks until their calc is resolved or a deadlock
  #    is detected

  @timeout 30_000

  # Public API

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

  @doc """
  Register a known field type from a resource.
  Called for attributes, explicitly-typed calculations, and aggregates.
  """
  def register_known_field(resource, field_name, type, constraints) do
    GenServer.call(__MODULE__, {:register_known, resource, field_name, type, constraints})
  end

  @doc """
  Register an `:auto` calculation that needs type resolution.

  `calc` is the `Ash.Resource.Calculation` struct.
  `dsl_state` is the resource's DSL state (needed for expression analysis).
  `deps` is the list of `{resource, field_name}` dependencies from the initial analysis.
  """
  def register_auto(resource, calc_name, calc, dsl_state, deps) do
    GenServer.call(__MODULE__, {:register_auto, resource, calc_name, calc, dsl_state, deps})
  end

  @doc """
  Signal that a resource has finished registering all its fields.
  """
  def done_registering(resource) do
    GenServer.call(__MODULE__, {:done_registering, resource})
  end

  @doc """
  Blocking call that waits for a calculation's type to be resolved.
  Returns `{:ok, type, constraints}` or `{:error, reason}`.
  """
  def resolve(resource, calc_name, timeout \\ @timeout) do
    GenServer.call(__MODULE__, {:resolve, resource, calc_name}, timeout)
  catch
    :exit, {:timeout, _} ->
      {:error,
       "Timed out waiting for type resolution of `#{calc_name}` on `#{inspect(resource)}`. " <>
         "This may indicate a circular dependency between `:auto` calculations across resources."}
  end

  @doc """
  Shut down the TypeResolver. Called after compilation is complete.
  """
  def shutdown do
    case Process.whereis(__MODULE__) do
      nil -> :ok
      _pid -> GenServer.stop(__MODULE__, :normal)
    end
  catch
    :exit, _ -> :ok
  end

  # GenServer callbacks

  @impl true
  def init(_) do
    {:ok,
     %{
       # %{{resource, field_name} => {type, constraints}}
       known_types: %{},
       # %{{resource, calc_name} => %{calc: calc, dsl_state: dsl_state, deps: [{resource, field}]}}
       pending: %{},
       # [{from, resource, calc_name}] - callers waiting for resolution
       waiters: [],
       # MapSet of resources that have finished registering
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
        # Not yet resolved - add to waiters
        state = %{state | waiters: [{from, resource, calc_name} | state.waiters]}
        state = run_resolution_pass(state)
        {:noreply, state}
    end
  end

  # Resolution logic

  defp run_resolution_pass(state) do
    {state, progress} = resolve_pending(state)

    if progress do
      # New resolutions may unblock others
      run_resolution_pass(state)
    else
      # No progress - check for deadlocks among waiters
      check_deadlocks(state)
    end
  end

  defp resolve_pending(state) do
    Enum.reduce(state.pending, {state, false}, fn {{resource, calc_name} = key, entry},
                                                   {state, progress} ->
      # Check if all deps are now in known_types
      unresolved = Enum.reject(entry.deps, &Map.has_key?(state.known_types, &1))

      if unresolved == [] do
        # All deps resolved! Determine the type.
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
            # Expression analysis found more deps we didn't know about
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
    # Only check for deadlocks if there are waiters with pending items
    waiting_keys =
      state.waiters
      |> Enum.map(fn {_from, resource, calc_name} -> {resource, calc_name} end)
      |> Enum.filter(&Map.has_key?(state.pending, &1))

    if waiting_keys == [] do
      state
    else
      # Build dependency graph and look for cycles
      case find_cycle(state.pending) do
        {:cycle, cycle} ->
          cycle_desc =
            cycle
            |> Enum.map(fn {resource, name} -> "#{inspect(resource)}.#{name}" end)
            |> Enum.join(" -> ")

          error =
            {:error,
             "Circular dependency detected between `:auto` calculations: #{cycle_desc}. " <>
               "At least one calculation in the cycle must have an explicit type."}

          # Reply to all waiters involved in the cycle
          cycle_set = MapSet.new(cycle)

          {cycle_waiters, remaining} =
            Enum.split_with(state.waiters, fn {_from, r, c} ->
              MapSet.member?(cycle_set, {r, c})
            end)

          Enum.each(cycle_waiters, fn {from, _, _} -> GenServer.reply(from, error) end)

          # Remove cycle entries from pending
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
      # Found a cycle - extract just the cycle portion
      cycle_start = Enum.find_index(path, &(&1 == node))
      cycle = Enum.slice(path, cycle_start..-1//1) ++ [node]
      {:cycle, cycle}
    else
      if MapSet.member?(visited, node) do
        :no_cycle
      else
        deps = Map.get(graph, node, [])
        # Only follow deps that are also pending (i.e., in the graph)
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

  defp get_expression(calc) do
    raise "`:auto` type is only supported for expression calculations (`expr(...)`), " <>
            "but calculation `#{calc.name}` uses `#{inspect(calc.calculation)}`"
  end
end
