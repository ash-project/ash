# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Planner do
  @moduledoc """
  Build an execution plan for a Reactor.

  Converts any unplanned steps into vertices in a graph with directed edges
  between them representing their dependencies (arguments).
  """

  alias Reactor.{Error.Internal.PlanError, Step}
  import Reactor, only: :macros
  import Reactor.Argument, only: :macros
  import Reactor.Utils

  @doc """
  Build an execution plan for a Reactor.

  Builds a graph of the step dependencies, resolves them and then builds an execution plan.
  """
  @spec plan(Reactor.t()) :: {:ok, Reactor.t()} | {:error, any}
  def plan(reactor) when not is_reactor(reactor),
    do: {:error, argument_error(:reactor, "not a Reactor", reactor)}

  def plan(reactor) when is_nil(reactor.plan),
    do: plan(%{reactor | plan: empty_graph()})

  def plan(reactor) do
    with {:ok, graph} <-
           reduce_steps_into_graph(reactor.plan, reactor.steps, reactor.intermediate_results),
         :ok <- assert_graph_not_cyclic(reactor, graph) do
      {:ok, %{reactor | steps: [], plan: graph}}
    end
  end

  @doc """
  Raising version of `plan/1`.
  """
  @spec plan!(Reactor.t()) :: Reactor.t() | no_return
  def plan!(reactor) do
    case plan(reactor) do
      {:ok, reactor} -> reactor
      {:error, reason} -> raise reason
    end
  end

  @doc false
  def get_ref(%{ref: ref}), do: ref

  defp empty_graph, do: Graph.new(type: :directed, vertex_identifier: &__MODULE__.get_ref/1)

  defp reduce_steps_into_graph(graph, steps, intermediate_results) do
    steps_by_name =
      graph
      |> Graph.vertices()
      |> Stream.filter(&is_struct(&1, Step))
      |> Stream.concat(steps)
      |> Map.new(&{&1.name, &1})

    steps
    |> reduce_while_ok(graph, fn
      step, graph when is_struct(step, Step) ->
        if Graph.has_vertex?(graph, step) do
          graph
          |> Graph.replace_vertex(step, step)
          |> reduce_arguments_into_graph(step, steps_by_name, intermediate_results)
        else
          graph
          |> Graph.add_vertex(step, step.name)
          |> reduce_arguments_into_graph(step, steps_by_name, intermediate_results)
        end

      not_step, _ ->
        {:error,
         PlanError.exception(
           graph: graph,
           step: not_step,
           message: "Value is not a `Reactor.Step` struct."
         )}
    end)
  end

  defp reduce_arguments_into_graph(graph, current_step, steps_by_name, intermediate_results) do
    # First handle regular arguments
    with {:ok, graph} <-
           add_regular_dependencies(graph, current_step, steps_by_name, intermediate_results) do
      # Then handle nested step dependencies
      add_nested_dependencies(graph, current_step, steps_by_name, intermediate_results)
    end
  end

  defp add_regular_dependencies(graph, current_step, steps_by_name, intermediate_results) do
    reduce_while_ok(current_step.arguments, graph, fn
      argument, graph
      when is_from_result(argument) and is_map_key(intermediate_results, argument.source.name) ->
        {:ok, graph}

      argument, graph when is_from_result(argument) ->
        dependency_name = argument.source.name

        case Map.fetch(steps_by_name, dependency_name) do
          {:ok, dependency} when dependency.name == current_step.name ->
            {:ok, graph}

          {:ok, dependency} ->
            {:ok,
             Graph.add_edge(graph, dependency, current_step,
               label: {:argument, argument.name, :for, current_step.name}
             )}

          :error ->
            {:error,
             PlanError.exception(
               graph: graph,
               step: current_step,
               message:
                 "Step `#{inspect(current_step.name)}` depends on the result of a step named `#{inspect(argument.source.name)}` which cannot be found"
             )}
        end

      argument, graph
      when is_from_input(argument) or is_from_value(argument) or is_from_element(argument) ->
        {:ok, graph}
    end)
  end

  defp add_nested_dependencies(graph, current_step, steps_by_name, intermediate_results) do
    nested_steps = Step.nested_steps(current_step)

    reduce_while_ok(nested_steps, graph, fn nested_step, graph ->
      add_nested_step_dependencies(
        graph,
        current_step,
        nested_step,
        steps_by_name,
        intermediate_results
      )
    end)
  end

  defp add_nested_step_dependencies(
         graph,
         containing_step,
         nested_step,
         steps_by_name,
         _intermediate_results
       ) do
    reduce_while_ok(nested_step.arguments, graph, fn
      argument, graph when is_from_result(argument) ->
        dependency_name = argument.source.name

        # Check if this is a cross-scope dependency (not in nested steps)
        case Map.fetch(steps_by_name, dependency_name) do
          {:ok, dependency} when dependency.name != containing_step.name ->
            # This is a cross-scope dependency - add it as a nested dependency edge
            {:ok,
             Graph.add_edge(graph, dependency, containing_step,
               label:
                 {:nested_dependency, nested_step.name, argument.name, :for, containing_step.name}
             )}

          _ ->
            # Either not found or is the containing step itself
            {:ok, graph}
        end

      argument, graph when is_from_element(argument) ->
        # For element references, we need to track them as nested dependencies
        # if they refer to a parent/outer map
        element_name = argument.source.name

        # Check if this element refers to a map step that's not the containing step
        case Map.fetch(steps_by_name, element_name) do
          {:ok, map_step} when map_step.name != containing_step.name ->
            # This is a cross-scope element reference - track it as a nested dependency
            # We'll need to pass the element value through the context
            {:ok,
             Graph.add_edge(graph, map_step, containing_step,
               label:
                 {:nested_element_dependency, nested_step.name, argument.name, element_name, :for,
                  containing_step.name}
             )}

          _ ->
            # Either the containing step itself or not found
            {:ok, graph}
        end

      _argument, graph ->
        {:ok, graph}
    end)
  end

  defp assert_graph_not_cyclic(reactor, graph) do
    if Graph.is_acyclic?(graph) do
      :ok
    else
      {:error,
       PlanError.exception(
         reactor: reactor,
         graph: graph,
         message: "Reactor contains cyclic dependencies."
       )}
    end
  end
end
