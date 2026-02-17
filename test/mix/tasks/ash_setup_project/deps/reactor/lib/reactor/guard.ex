# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Guard do
  @moduledoc """
  A Reactor guard.

  Guard types should implement the `Reactor.Guard.Build` protocol.

  This struct contains a single two arity function, which when called with a
  step's arguments and context returns one of the following:

  - `:cont` - this guard has passed - continue evaluating any additional guards
    and if exhausted run the step.
  - `{:halt, result}` - the guard has failed - use `result` as the steps result.
  """

  defstruct description: nil, fun: nil

  @type t :: %__MODULE__{
          description: nil | String.t(),
          fun:
            (Reactor.inputs(), Reactor.context() -> :cont | {:halt, Reactor.Step.run_result()})
            | mfa
        }

  defimpl Reactor.Guard.Build do
    def build(guard), do: {:ok, [guard]}
  end
end
