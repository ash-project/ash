# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.CompensateStepError do
  @moduledoc """
  This error is returned when an error occurs during step compensation.

  Its `error` key will contain the error that was raised or returned by the
  `c:Step.compensate/4` callback.
  """

  use Reactor.Error, fields: [:error, :reactor, :step], class: :invalid

  @doc false
  @impl true
  def message(error) do
    """
    # Compensate Step Error

    An error occurred while attempting to compensate the `#{inspect(error.step.name)}` step.

    ## `step`:

    #{inspect(error.step)}

    ## `error`:

    #{describe_error(error.error)}
    """
  end
end
