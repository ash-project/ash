# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.InvalidResultError do
  @moduledoc """
  This error is returned when a step returns an invalid result.
  """

  use Reactor.Error, fields: [:reactor, :step, :result, :arguments], class: :invalid

  @doc false
  def message(error) do
    """
    # Invalid Result Error

    The step `#{inspect(error.step.name)}` returned an invalid result.

    Valid return types from the `c:Reactor.Step.run/3` callback are:

    - `{:ok, any}` - a successful result.
    - `{:ok, any, [Reactor.Step.t]}` - a successful result with additional steps to
      add to the running reactor.
    - `:retry` - the step wants to be retried.
    - `{:retry, Exception.t | any}` - the step wants to be retried, and here's why.
    - `{:error, Exception.t | any}` - the step failed, and here's why.
    - `{:halt, any}` - the step wants the Reactor to stop.

    ## `result`:

    #{inspect(error.result)}

    ## `step`:

    #{inspect(error.step)}

    ## `arguments`:

    #{inspect(error.arguments)}
    """
  end
end
