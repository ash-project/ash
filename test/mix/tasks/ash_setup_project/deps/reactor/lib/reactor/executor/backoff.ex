# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Executor.Backoff do
  @moduledoc """
  A placeholder graph vertex for steps which are waiting for their backoff to expire.
  """
  defstruct ref: nil, expires_at: nil

  @type t :: %__MODULE__{
          ref: reference(),
          expires_at: pos_integer()
        }

  @doc """
  Generate a backoff which expires in `delays` milliseconds.
  """
  @spec delay(delay :: pos_integer()) :: t
  def delay(delay),
    do: %__MODULE__{ref: make_ref(), expires_at: System.monotonic_time(:millisecond) + delay}
end
