defmodule Ash.Resource.Validation.Function do
  @moduledoc false

  use Ash.Resource.Validation

  @impl true
  def validate(changeset, [{:fun, {m, f, a}}]) do
    apply(m, f, [changeset | a])
  end

  @impl true
  def validate(changeset, [{:fun, fun}]) do
    fun.(changeset)
  end

  @impl true
  def describe(opts) do
    [
      message: "must pass function %{function}",
      vars: [function: opts[:fun]]
    ]
  end
end
