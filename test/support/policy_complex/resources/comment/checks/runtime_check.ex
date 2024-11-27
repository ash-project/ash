defmodule Ash.Test.Support.PolicyComplex.Comment.Checks.RuntimeCheck do
  @moduledoc false
  use Ash.Policy.Check

  def describe(_) do
    "sends a telemetry event when called"
  end

  def strict_check(_, _, _) do
    {:ok, :unknown}
  end

  def check(_, items, _, _) do
    :telemetry.execute([:ash, :test, :runtime_check_executed], %{}, %{items: items})

    items
  end
end
