# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.ReturnArgument do
  @moduledoc """
  A very simple step which simply returns the named argument, if provided.

  ## Options.

  * `argument` - the name of the argument to return.
  """

  use Reactor.Step

  @doc false
  @impl true
  def run(arguments, _, options) do
    with {:ok, argument} <- Keyword.fetch(options, :argument),
         {:ok, value} <- Map.fetch(arguments, argument) do
      {:ok, value}
    else
      :error -> {:error, "Unable to find argument"}
    end
  end
end
