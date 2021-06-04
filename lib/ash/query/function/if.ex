defmodule Ash.Query.Function.If do
  @moduledoc """
  If predicate is truthy, then the second argument is returned, otherwise the third.
  """
  use Ash.Query.Function, name: :if

  def args, do: [[:boolean, :any, :any]]

  def evaluate(%{arguments: [condition, when_true, when_false]}) do
    if condition do
      {:known, when_true}
    else
      {:known, when_false}
    end
  end
end
