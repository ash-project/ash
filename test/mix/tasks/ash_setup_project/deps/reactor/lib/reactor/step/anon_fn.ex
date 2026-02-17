# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.AnonFn do
  @moduledoc """
  The built-in step for executing in-line DSL anonymous functions.

  ## Options

  * `run` - a one or two arity function or MFA which will be called as the run
    function of the step.
  * `compensate` - a one to three arity function or MFA which will be called as
    the compensate function of the step.  Optional.
  * `undo` - a one to three arity function or MFA which will be called as the
    undo function of this step.  Optional.
  * `backoff` - a one to three arity function or MFA which will be called as
    the backoff function of the step. Optional.
  """

  use Reactor.Step
  @behaviour Reactor.Mermaid

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), keyword) :: {:ok | :error, any}
  def run(arguments, context, options) do
    case Keyword.fetch!(options, :run) do
      fun when is_function(fun, 1) ->
        fun.(arguments)

      fun when is_function(fun, 2) ->
        fun.(arguments, context)

      {m, f, a} when is_atom(m) and is_atom(f) and is_list(a) ->
        apply(m, f, [arguments, context] ++ a)
    end
  end

  @doc false
  @impl true
  @spec compensate(any, Reactor.inputs(), Reactor.context(), keyword) ::
          {:continue, any} | :ok | :retry
  def compensate(reason, arguments, context, options) do
    case Keyword.fetch(options, :compensate) do
      {:ok, fun} when is_function(fun, 1) ->
        fun.(reason)

      {:ok, fun} when is_function(fun, 2) ->
        fun.(reason, arguments)

      {:ok, fun} when is_function(fun, 3) ->
        fun.(reason, arguments, context)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        apply(m, f, [reason, arguments, context] ++ a)

      _ ->
        :ok
    end
  end

  @doc false
  @impl true
  @spec undo(any, Reactor.inputs(), Reactor.context(), keyword) :: :ok | :retry | {:error, any}
  def undo(value, arguments, context, options) do
    case Keyword.fetch(options, :undo) do
      {:ok, fun} when is_function(fun, 1) ->
        fun.(value)

      {:ok, fun} when is_function(fun, 2) ->
        fun.(value, arguments)

      {:ok, fun} when is_function(fun, 3) ->
        fun.(value, arguments, context)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        apply(m, f, [value, arguments, context] ++ a)

      _ ->
        :ok
    end
  end

  @doc false
  @impl true
  @spec backoff(any, Reactor.inputs(), Reactor.context(), keyword) :: :now | pos_integer()
  def backoff(reason, arguments, context, options) do
    case Keyword.fetch(options, :backoff) do
      {:ok, fun} when is_function(fun, 1) ->
        fun.(reason)

      {:ok, fun} when is_function(fun, 2) ->
        fun.(reason, arguments)

      {:ok, fun} when is_function(fun, 3) ->
        fun.(reason, arguments, context)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        apply(m, f, [reason, arguments, context] ++ a)

      _ ->
        :now
    end
  end

  @doc false
  @impl true
  def can?(%{impl: {_, opts}}, :undo), do: is_function(Keyword.get(opts, :undo))
  def can?(%{impl: {_, opts}}, :compensate), do: is_function(Keyword.get(opts, :compensate))
  def can?(_, _), do: false

  @doc false
  @impl true
  def to_mermaid(step, options),
    do: __MODULE__.Mermaid.to_mermaid(step, options)
end
