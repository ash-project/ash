defmodule Ash.Query.Function.If do
  @moduledoc """
  If predicate is truthy, then the second argument is returned, otherwise the third.
  """
  use Ash.Query.Function, name: :if

  def args, do: [[:boolean, :any], [:boolean, :any, :any]]

  def new([condition, block]) do
    if Keyword.keyword?(block) && Keyword.has_key?(block, :do) do
      if Keyword.has_key?(block, :else) do
        if condition == true do
          {:ok, block[:do]}
        else
          if condition == false do
            {:ok, block[:else]}
          else
            super([condition, block[:do], block[:else]])
          end
        end
      else
        if condition == true do
          {:ok, block[:do]}
        else
          if condition == false do
            {:ok, nil}
          else
            super([condition, block[:do], nil])
          end
        end
      end
    else
      super([condition, block, nil])
    end
  end

  def new(other) do
    super(other)
  end

  def evaluate(%{arguments: [condition, when_true, when_false]}) do
    if condition do
      {:known, when_true}
    else
      {:known, when_false}
    end
  end
end
