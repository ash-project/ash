defmodule Ash.Query.Function.IsNil do
  @moduledoc """
  true if the provided field is nil
  """
  use Ash.Query.Function, name: :is_nil

  def args, do: [[:any]]

  def returns, do: [:boolean]

  def evaluate_nil_inputs?, do: true

  def new([arg]) do
    Ash.Query.Operator.new(Ash.Query.Operator.IsNil, arg, true)
  end

  def evaluate(%{arguments: [val]}) do
    {:known, is_nil(val)}
  end

  def can_return_nil?(_), do: false
end
