# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Unknown.UnknownError do
  @moduledoc """
  An error used to wrap unknown errors.
  """

  use Reactor.Error, fields: [:error], class: :unknown

  @doc false
  @impl true
  def message(error) do
    """
    # Unknown Error

    An unknown error occurred.

    ## `error`:

    #{describe_error(error.error)}
    """
  end
end
