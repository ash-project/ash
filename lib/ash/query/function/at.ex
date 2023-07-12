defmodule Ash.Query.Function.At do
  @moduledoc """
  Gets an element in the list by index
  """

  use Ash.Query.Function, name: :at

  def args, do: [[{:array, :any}, :integer]]

  def evaluate(%{arguments: [list, at]}) do
    {:known, Enum.at(list, at)}
  end
end
