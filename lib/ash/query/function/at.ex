defmodule Ash.Query.Function.At do
  @moduledoc """
  Gets an element in the list by index
  """

  use Ash.Query.Function, name: :at

  def args, do: [[{:array, :any}, :integer]]

  def returns, do: [:same]

  def evaluate(%{arguments: [list, at]}) do
    {:known, Enum.at(list, at)}
  end

  def can_return_nil?(%{arguments: [value | _]}) do
    Ash.Expr.can_return_nil?(value)
  end
end
