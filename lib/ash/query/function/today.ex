defmodule Ash.Query.Function.Today do
  @moduledoc """
  Returns the current date.
  """

  use Ash.Query.Function, name: :today, eager_evaluate?: false

  def args, do: [[]]

  def returns, do: [:date]

  def evaluate(_), do: {:known, Date.utc_today()}

  def can_return_nil?(_), do: false
end
