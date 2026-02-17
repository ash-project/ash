# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.UndoStepError do
  @moduledoc """
  This error is returned when an error occurs when attempting to undo step execution.

  Its `error` key will contain the error that was raised or returned by the
  `c:Step.undo/4` callback.
  """
  use Reactor.Error, fields: [:step, :error], class: :invalid

  @doc false
  @impl true
  def message(error) do
    """
    # Undo Step Error

    An error occurred while attempting to undo the step `#{inspect(error.step.name)}`.

    ## `step`:

    #{inspect(error.step)}

    ## `error`:

    #{describe_error(error.error)}
    """
  end
end
