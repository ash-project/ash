# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeUpper do
  @moduledoc """
  Returns a range's upper endpoint, of its inner type.

  Nil where the range is unbounded at that end, and where it is empty.

     range_upper(range)
  """
  use Ash.Query.Function, name: :range_upper

  alias Ash.Range

  def args, do: [[:any]]

  def returns, do: [:any]

  def evaluate(%{arguments: [nil]}), do: {:known, nil}
  def evaluate(%{arguments: [%Range{upper: upper}]}), do: {:known, upper}
  def evaluate(_other), do: :unknown

  def can_return_nil?(_), do: true
end
