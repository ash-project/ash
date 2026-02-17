# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Middleware do
  @moduledoc """
  The Middleware behaviour.

  By implementing this behaviour you can modify the internal state of the
  Reactor during startup, execution and shutdown.

  Middlewares can be added to the reactor either with the `middlewares` DSL
  section or by the `add_middleware/2`, etc, functions in `Reactor.Builder`.

  All callbacks in this behaviour are optional so you can define only the parts
  you need for your feature.
  """

  @type t :: module

  @type context :: Reactor.context()
  @type result :: any
  @type error_or_errors :: Exception.t() | [Exception.t()]

  @type step_event ::
          :compensate_complete
          | :compensate_retry
          | :run_retry
          | :undo_complete
          | :undo_retry
          | :undo_start
          | {:compensate_continue, any}
          | {:compensate_error, error_or_errors}
          | {:compensate_retry, any}
          | {:compensate_start, any}
          | {:guard_start, Reactor.Guard.t(), arguments :: Reactor.inputs()}
          | {:guard_fail, Reactor.Guard.t(), Reactor.Step.run_result()}
          | {:guard_pass, Reactor.Guard.t()}
          | {:process_start, pid}
          | {:process_terminate, pid}
          | {:run_complete, result}
          | {:run_error, error_or_errors()}
          | {:run_halt, any}
          | {:run_retry, any}
          | {:run_start, arguments :: Reactor.inputs()}
          | {:undo_error, error_or_errors()}
          | {:undo_retry, any}

  @doc """
  The complete callback will be called with the successful result of the
  Reactor.

  This gives you the opportunity to modify the return value or to perform clean
  up of any non-reactor-managed resources (eg notifications).

  Note that these callbacks are called in an arbitrary order meaning that the
  result value passed may have already been altered by another callback.

  If any callback returns an error then any remaining callbacks will not
  be called.
  """
  @callback complete(result, context) :: {:ok, result} | {:error, any}

  @doc """
  The error callback will be called the final error value(s) of the Reactor.

  This gives you the opportunity to modify the return value or to perform clean
  up of any non-reactor-managed resources (eg notifications).

  Note that these callbacks are called in an arbitrary order meaning that the
  error value passed may have already been altered by another callback.

  Here a return value of `:ok` will continue calls to other callbacks without
  modifying the error value.
  """
  @callback error(error_or_errors, context) :: :ok | {:error, any}

  @doc """
  The halt callback will be called with the Reactor context when halting.

  This allows you to clean up any non-reactor-managed resources or modify the
  context for later re-use by a future `init/1` callback.
  """
  @callback halt(context) :: {:ok, context} | {:error, any}

  @doc """
  The init callback will be called with the Reactor context when starting up.

  This gives you the opportunity to modify the context or to perform any
  initialisation of any non-reactor-managed resources (eg notifications).
  """
  @callback init(context) :: {:ok, context} | {:error, any}

  @doc """
  Called before starting an asynchronous step in order to retrieve any context
  information that needs to be passed into the new process.

  This is most likely used by tracers to copy span information from the parent
  process to the child process.
  """
  @callback get_process_context :: any

  @doc """
  Called inside a new asynchronous step in order to set context information into
  the new process.

  This is most likely used by tracers to copy span information from the parent
  process to the child process.
  """
  @callback set_process_context(any) :: :ok

  @doc """
  Receive events about the execution of the Reactor.

  This callback will block the Reactor, so it's encouraged that you do anything
  expensive in another process.
  """
  @callback event(step_event, Reactor.Step.t(), Reactor.context()) :: :ok

  @optional_callbacks complete: 2,
                      error: 2,
                      event: 3,
                      halt: 1,
                      init: 1,
                      get_process_context: 0,
                      set_process_context: 1

  defmacro __using__(_env) do
    quote do
      @behaviour unquote(__MODULE__)
    end
  end
end
