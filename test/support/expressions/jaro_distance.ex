defmodule Ash.Test.Expressions.JaroDistance do
  @moduledoc false
  use Ash.CustomExpression,
    name: :jaro_distance,
    arguments: [
      [:string, :string]
    ]

  def expression(data_layer, [left, right])
      when data_layer in [
             Ash.DataLayer.Ets,
             Ash.DataLayer.Simple
           ] do
    {:ok, expr(fragment(&String.jaro_distance/2, ^left, ^right))}
  end

  def expression(_data_layer, _args), do: :unknown
end
