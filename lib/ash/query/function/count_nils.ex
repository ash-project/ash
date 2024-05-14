defmodule Ash.Query.Function.CountNils do
  @moduledoc """
  Returns the count of nil.

      count_nil([nil, 1, nil]) # 2
  """
  use Ash.Query.Function, name: :count_nils

  def args, do: [[{:array, :any}]]

  def evaluate(%{arguments: [val]}) when is_list(val) do
    {:known, Enum.count(val, &is_nil/1)}
  end

  def evaluate(%{arguments: [nil]}) do
    {:known, nil}
  end

  def evaluate(_) do
    {:error, "Cannot use count_nils/1 on non-list inputs"}
  end

  def can_return_nil?(%{arguments: [date | _]}) do
    Ash.Expr.can_return_nil?(date)
  end
end
