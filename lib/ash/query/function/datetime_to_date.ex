defmodule Ash.Query.Function.DateTimeToDate do
  @moduledoc """
  Convert a DateTime into Date.

  For example:
     datetime_to_date(~U[2024-10-15 13:52:23.512200Z])
  """

  use Ash.Query.Function,
    name: :datetime_to_date,
    eager_evaluate?: false

  def args, do: [[:datetime], [:naive_datetime]]

  def returns, do: [:date, :date]

  def evaluate(%{arguments: [%DateTime{} = datetime]}) do
    {:known, DateTime.to_date(datetime)}
  end

  def evaluate(%{arguments: [%NaiveDateTime{} = datetime]}) do
    {:known, NaiveDateTime.to_date(datetime)}
  end

  def evaluate(%{arguments: [nil]}) do
    {:known, nil}
  end

  def can_return_nil?(%{arguments: [datetime]}) do
    Ash.Expr.can_return_nil?(datetime)
  end
end
