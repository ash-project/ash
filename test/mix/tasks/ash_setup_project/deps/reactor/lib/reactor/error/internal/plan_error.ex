# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Error.Internal.PlanError do
  @moduledoc """
  This error is returned when the step graph cannot be built.
  """

  use Reactor.Error,
    fields: [:graph, :message, :reactor, :step],
    class: :reactor

  @doc false
  @impl true
  def message(error) do
    [
      """
      # Reactor Plan Error

      An error occurred while building or updating the Reactor execution graph.

      #{error.message}
      """
    ]
    |> maybe_append_result(fn ->
      if error.reactor do
        """
        ## Reactor

        ```
        #{inspect(error.reactor)}
        ```
        """
      end
    end)
    |> maybe_append_result(fn ->
      if error.step do
        """
        ## Step

        ```
        #{inspect(error.step)}
        ```
        """
      end
    end)
    |> maybe_append_result(fn ->
      if error.graph do
        """
        ## Graph

        ```
        #{inspect(error.graph)}
        ```
        """
      end
    end)
    |> Enum.join("\n")
  end
end
