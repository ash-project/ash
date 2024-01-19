defmodule Ash.Resource.Calculation.RuntimeExpression do
  @moduledoc false
  use Ash.Calculation

  def calculate(records, opts, context) do
    Ash.Resource.Calculation.Expression.calculate(records, opts, context)
  end
end
