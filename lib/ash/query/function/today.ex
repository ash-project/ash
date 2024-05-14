defmodule Ash.Query.Function.Today do
  @moduledoc """
  Returns the current datetime
  """
  use Ash.Query.Function, name: :today, eager_evaluate?: false

  def args, do: [[]]

  def evaluate(_), do: {:known, Date.utc_today()}

  def can_return_nil?(_), do: false
end
