defmodule Ash.Query.Function.Lazy do
  @moduledoc """
  Runs the provided MFA and returns the result as a known value.

  Evaluated just before running the query.
  """
  use Ash.Query.Function, name: :lazy, eager_evaluate?: false

  def args, do: [[:any]]

  def evaluate(%{arguments: [{m, f, a}]}), do: {:known, apply(m, f, a)}
end
