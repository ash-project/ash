# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Compose do
  @moduledoc """
  A built-in step which can embed one reactor inside another.

  This step calls `Reactor.run/3` on the inner reactor and returns it's result.
  Reactor will correctly share the concurrency availability over both the parent
  and child Reactors.
  """

  use Reactor.Step
  alias Reactor.{Builder, Step}
  @behaviour Reactor.Mermaid

  @doc false
  @impl true
  def run(arguments, %{current_step: %{name: {:compose, _inner}}} = context, options) do
    perform_reactor_run(arguments, context, options)
  end

  def run(arguments, context, options) do
    if options[:support_undo?] do
      schedule_undoable_reactor_run(context)
    else
      perform_reactor_run(arguments, context, options)
    end
  end

  @doc false
  @impl true
  def can?(%{name: {:compose, _}, impl: {__MODULE__, opts}}, :undo),
    do: opts[:support_undo?] || false

  def can?(_, _), do: false

  @doc false
  @impl true
  def undo(%{reactor: reactor}, _args, context, _options) do
    Reactor.undo(reactor, context, concurrency_key: context.concurrency_key)
  end

  def schedule_undoable_reactor_run(context) do
    {:ok, :pending_result,
     [
       %{context.current_step | name: {:compose, context.current_step.name}, ref: make_ref()},
       %{
         Builder.new_step!(
           context.current_step.name,
           {Step.AnonFn, run: {__MODULE__, :extract_result, []}},
           [composed: {:result, {:compose, context.current_step.name}}],
           async?: context[:async?] || true,
           max_retries: 0
         )
         | ref: context.current_step.ref
       }
     ]}
  end

  def perform_reactor_run(arguments, context, options) do
    reactor = Keyword.fetch!(options, :reactor)
    allow_async? = Keyword.get(options, :allow_async?, true)

    # Child reactor can only run async if both parent allows async AND allow_async? is true
    # Use the context.async? field which contains the parent reactor's async state
    parent_async? = Map.get(context, :async?, true)
    child_async? = parent_async? and allow_async?

    Reactor.run(reactor, arguments, context,
      concurrency_key: context.concurrency_key,
      async?: child_async?,
      fully_reversible?: options[:support_undo?]
    )
    |> case do
      {:ok, result} ->
        {:ok, result}

      {:ok, result, reactor} ->
        {:ok, %{result: result, reactor: reactor}}

      {:error, reason} ->
        {:error, reason}

      {:halted, reactor} ->
        {:halt, reactor}
    end
  end

  @doc false
  @impl true
  def to_mermaid(step, options),
    do: __MODULE__.Mermaid.to_mermaid(step, options)

  def extract_result(%{composed: %{result: result}}, _), do: {:ok, result}
  def extract_result(_, _), do: {:ok, nil}
end
