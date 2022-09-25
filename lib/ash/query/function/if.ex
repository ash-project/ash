defmodule Ash.Query.Function.If do
  @moduledoc """
  If predicate is truthy, then the second argument is returned, otherwise the third.
  """
  use Ash.Query.Function, name: :if

  def args, do: [[:boolean, :any], [:boolean, :any, :any]]

  def new([condition, block]) do
    if Keyword.keyword?(block) && Keyword.has_key?(block, :do) do
      if Keyword.has_key?(block, :else) do
        super([condition, block[:do], block[:else]])
      else
        super([condition, block[:do], nil])
      end
    else
      super([condition, block, nil])
    end
  end

  def new(other) do
    super(other)
  end

  def evaluate(%{arguments: [true, when_true, _]}),
    do: {:known, when_true}

  def evaluate(%{arguments: [false, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [nil, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [_, when_true, _]}), do: {:known, when_true}
end
