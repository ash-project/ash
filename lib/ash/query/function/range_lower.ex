# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeLower do
  @moduledoc """
  Returns a range's lower endpoint, of its inner type.

  Nil where the range is unbounded at that end, and where it is empty.

     range_lower(range)
  """
  use Ash.Query.Function, name: :range_lower

  alias Ash.Range

  def args, do: [[:any]]

  def returns, do: [:any]

  def evaluate(%{arguments: [nil]}), do: {:known, nil}
  def evaluate(%{arguments: [%Range{lower: lower}]}), do: {:known, lower}
  def evaluate(_other), do: :unknown

  def can_return_nil?(_), do: true
end
