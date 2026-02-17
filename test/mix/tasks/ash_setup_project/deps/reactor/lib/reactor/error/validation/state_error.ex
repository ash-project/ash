# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Validation.StateError do
  @moduledoc """
  An error returned when a Reactor is in an unexpected state.
  """

  use Reactor.Error,
    fields: [:reactor, :state, :expected],
    class: :validation

  @doc false
  @impl true
  def message(error) do
    """
    # Reactor State Error

    #{state_message(error)}
    """
  end

  defp state_message(%{expected: [], state: state}),
    do: "Reactor is in an invalid state: `#{inspect(state)}`"

  defp state_message(%{expected: [expected], state: state}),
    do: "Reactor is in an invalid state: `#{inspect(state)}`, expected: `#{inspect(expected)}`"

  defp state_message(error) do
    valid_states =
      error.expected
      |> Enum.map_join("\n", &"  * `#{inspect(&1)}`")

    """
    Reactor is in an invalid state: `#{inspect(error.state)}`

    Expected states:

    #{valid_states}
    """
  end
end
