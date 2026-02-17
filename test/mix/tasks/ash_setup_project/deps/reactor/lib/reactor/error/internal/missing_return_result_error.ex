# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Internal.MissingReturnResultError do
  @moduledoc """
  This error is returned when the Reactor's return name doesn't match any of the
  known step results.
  """

  use Reactor.Error,
    fields: [:reactor],
    class: :reactor

  @doc false
  @impl true
  def message(error) do
    intermediate_keys =
      error.reactor.intermediate_values
      |> Map.keys()

    known_results =
      intermediate_keys
      |> Enum.map_join("\n", &"  * `#{inspect(&1)}`")

    """
    # Missing Return Result Error

    The Reactor was asked to return a result named `#{inspect(error.reactor.return)}`, however an intermediate result with that name is missing.
    #{did_you_mean?(error.reactor.return, intermediate_keys)}

    ## Intermediate results:

    #{known_results}
    """
  end
end
