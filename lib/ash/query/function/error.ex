# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Error do
  @moduledoc """
  If the predicate is truthy, the provided exception is raised with the provided values.

  This exception is not "raised" in the Elixir sense, but the entire expression fails to
  evaluate with the given error. Various data layers will handle this differently.
  """
  use Ash.Query.Function, name: :error, eager_evaluate?: false

  def args, do: [[:atom, :any]]

  def returns, do: :no_return

  def evaluate(%{arguments: [exception, input]}) do
    {:error, exception.exception(Keyword.new(input))}
  end
end
