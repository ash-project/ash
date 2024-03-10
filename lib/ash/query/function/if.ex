defmodule Ash.Query.Function.If do
  @moduledoc """
  If predicate is truthy, then the second argument is returned, otherwise the third.
  """
  use Ash.Query.Function, name: :if
  import Ash.Expr, only: [expr?: 1]

  def args, do: [[:boolean, :any], [:boolean, :any, :any]]

  def evaluate_nil_inputs?, do: true

  def new([condition, block]) do
    args =
      if Keyword.keyword?(block) && Keyword.has_key?(block, :do) do
        if Keyword.has_key?(block, :else) do
          [condition, block[:do], block[:else]]
        else
          [condition, block[:do], nil]
        end
      else
        [condition, block, nil]
      end

    new(args)
  end

  def new([true, block, _else_block]), do: {:ok, block}
  def new([false, _block, else_block]), do: {:ok, else_block}
  def new([nil, _block, else_block]), do: {:ok, else_block}

  def new([condition, block, else_block]) do
    super([condition, block, else_block])
  end

  def evaluate(%{arguments: [true, when_true, _]}),
    do: {:known, when_true}

  def evaluate(%{arguments: [false, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [nil, _, when_false]}),
    do: {:known, when_false}

  def evaluate(%{arguments: [_, when_true, _]}), do: {:known, when_true}

  def partial_evaluate(%{arguments: [false, _, when_false]}),
    do: when_false

  def partial_evaluate(%{arguments: [nil, _, when_false]}),
    do: when_false

  def partial_evaluate(%{arguments: [condition, when_true, _]} = fun) do
    if expr?(condition) do
      fun
    else
      when_true
    end
  end
end
