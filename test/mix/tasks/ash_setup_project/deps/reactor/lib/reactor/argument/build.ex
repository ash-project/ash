# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defprotocol Reactor.Argument.Build do
  @moduledoc """
  A protocol which can be used to convert something into an argument.
  """

  alias Reactor.Argument

  @doc """
  Convert the input into one or more arguments.
  """
  @spec build(t) :: {:ok, [Argument.t()]} | {:error, any}
  def build(input)
end

defimpl Reactor.Argument.Build, for: Reactor.Argument do
  alias Reactor.Argument

  def build(argument), do: {:ok, [argument]}
end

defimpl Reactor.Argument.Build, for: Tuple do
  alias Reactor.Argument
  import Reactor.Utils

  def build({name, {:input, source}}), do: {:ok, [Argument.from_input(name, source)]}
  def build({name, {:result, source}}), do: {:ok, [Argument.from_result(name, source)]}
  def build({name, value}), do: {:ok, [Argument.from_value(name, value)]}
  def build(tuple), do: {:error, argument_error(:tuple, "contains a non-argument value", tuple)}
end
