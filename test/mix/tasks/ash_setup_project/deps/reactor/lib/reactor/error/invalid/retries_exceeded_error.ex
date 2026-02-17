# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Invalid.RetriesExceededError do
  @moduledoc """
  This error is returned when a step attempts to retry more times that is
  allowed.
  """
  use Reactor.Error, fields: [:retry_count, :step], class: :invalid

  @doc false
  @impl true
  def message(error) do
    """
    # Retries Exceeded Error

    Maximum number of retries exceeded executing step `#{inspect(error.step.name)}`.

    ## `retry_count`:

    #{inspect(error.retry_count)}

    ## `step`:

    #{inspect(error.step)}
    """
  end
end
