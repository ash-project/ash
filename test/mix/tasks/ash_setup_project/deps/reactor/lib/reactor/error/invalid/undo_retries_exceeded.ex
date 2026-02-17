# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.UndoRetriesExceededError do
  @moduledoc """
  An error used when a step runs out of retry events and no other error is
  thrown.
  """
  use Reactor.Error, fields: [:step, :retry_count], class: :invalid

  @doc false
  @impl true
  def message(error) do
    """
    # Undo Retries Exceeded Error

    Maximum number of retries exceeded while attempting to undo step `#{inspect(error.step.name)}`.

    ## `retry_count`:

    #{inspect(error.retry_count)}

    ## `step`:

    #{inspect(error.step)}
    """
  end
end
