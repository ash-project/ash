# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.Hooks do
  @moduledoc """
  Handles the execution of reactor middleware hooks.
  """
  alias Reactor.{Middleware, Utils}
  require Logger

  @doc "Run the init hooks collecting the new context as it goes"
  @spec init(Reactor.t(), Reactor.context()) :: {:ok, Reactor.context()} | {:error, any}
  def init(reactor, context) do
    context =
      Map.put(context, :__reactor__, %{
        id: reactor.id,
        run_id: context.run_id,
        inputs: reactor.inputs,
        middleware: reactor.middleware,
        step_count: step_count(reactor),
        initial_state: reactor.state
      })

    reactor.middleware
    |> Utils.reduce_while_ok(context, fn middleware, context ->
      Code.ensure_loaded!(middleware)

      if function_exported?(middleware, :init, 1) do
        middleware.init(context)
      else
        {:ok, context}
      end
    end)
  end

  @doc "Run the halt hooks collecting the new context as it goes"
  @spec halt(Reactor.t(), Reactor.context()) :: {:ok, Reactor.context()} | {:error, any}
  def halt(reactor, context) do
    Utils.reduce_while_ok(reactor.middleware, context, fn middleware, context ->
      if function_exported?(middleware, :halt, 1) do
        middleware.halt(context)
      else
        {:ok, context}
      end
    end)
  end

  @doc "Run the completion hooks allowing the result to be replaced"
  @spec complete(Reactor.t(), any, Reactor.context()) :: {:ok, any} | {:error, any}
  def complete(reactor, result, context) do
    Utils.reduce_while_ok(reactor.middleware, result, fn middleware, result ->
      if function_exported?(middleware, :complete, 2) do
        middleware.complete(result, context)
      else
        {:ok, result}
      end
    end)
  end

  @doc "Run the error hooks allowing the error to be replaced"
  @spec error(Reactor.t(), any, Reactor.context()) :: {:error, any}
  def error(reactor, reason, context) do
    Enum.reduce(reactor.middleware, {:error, reason}, fn middleware, {:error, reason} ->
      with true <- function_exported?(middleware, :error, 2),
           {:error, reason} <- middleware.error(reason, context) do
        {:error, reason}
      else
        _ -> {:error, reason}
      end
    end)
  end

  @doc "Run any get_process_context hooks"
  @spec get_process_contexts(Reactor.t()) :: %{optional(Middleware.t()) => any}
  def get_process_contexts(reactor) do
    Enum.reduce(reactor.middleware, %{}, fn middleware, result ->
      if function_exported?(middleware, :get_process_context, 0) do
        result
        |> Map.put(middleware, middleware.get_process_context())
      else
        result
      end
    end)
  end

  @doc "Run set_process_context hooks given the result of `get_process_contexts/1`"
  @spec set_process_contexts(%{optional(Middleware.t()) => any}) :: :ok
  def set_process_contexts(contexts) do
    for {middleware, context} <- contexts do
      if function_exported?(middleware, :set_process_context, 1) do
        middleware.set_process_context(context)
      else
        Logger.warning(
          "Unable to set process context for middleware `#{inspect(middleware)}`: `set_process_context/1` is not defined."
        )
      end
    end

    :ok
  end

  @doc "Emit a step event."
  @spec event(Reactor.t(), Middleware.step_event(), Reactor.Step.t(), Reactor.context()) :: :ok
  def event(reactor, event, step, context) do
    for middleware <- reactor.middleware do
      if function_exported?(middleware, :event, 3) do
        middleware.event(event, step, context)
      end
    end

    :ok
  end

  defp step_count(reactor) when is_nil(reactor.plan), do: length(reactor.steps)

  defp step_count(reactor) do
    vertices = Graph.num_vertices(reactor.plan)
    length(reactor.steps) + vertices
  end
end
