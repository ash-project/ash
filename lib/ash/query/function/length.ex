defmodule Ash.Query.Function.Length do
  @moduledoc """
  Returns the length of a list attribute defined by the composite type `{:array, Type}`.

      length(roles)

  If the attribute allows nils:

      length(roles || [])

  """
  use Ash.Query.Function, name: :length

  def args, do: [[{:array, :any}]]

  def returns, do: [:integer]

  def evaluate(%{arguments: [val]}) when is_list(val) do
    {:known, Enum.count(val)}
  end

  def evaluate(%{arguments: [nil]}) do
    {:known, nil}
  end

  def evaluate(_) do
    {:error, "Cannot use length/1 on non-list inputs"}
  end

  def can_return_nil?(%{arguments: [val]}) do
    Ash.Expr.can_return_nil?(val)
  end
end
