# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defprotocol Reactor.Guard.Build do
  @moduledoc """
  A protocol which can be used to convert something into a guard.
  """

  alias Reactor.Guard

  @doc """
  Convert the input into one or more guards.
  """
  @spec build(t) :: {:ok, [Guard.t()]} | {:error, any}
  def build(input)
end
