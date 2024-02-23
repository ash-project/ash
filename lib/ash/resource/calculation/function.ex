defmodule Ash.Resource.Calculation.Function do
  @moduledoc false
  use Ash.Resource.Calculation

  def calculate(results, [fun: fun], context) do
    apply_fun(fun, results, context)
  end

  defp apply_fun({m, f, a}, results, context) do
    apply(m, f, [results, context | a])
  end

  defp apply_fun(fun, results, context) do
    fun.(results, context)
  end
end
