# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.ReturnAllArguments do
  @moduledoc """
  A very simple step which simply returns all it's arguments unchanged.
  """

  use Reactor.Step

  @doc false
  @impl true
  def run(arguments, _, _), do: {:ok, arguments}
end
