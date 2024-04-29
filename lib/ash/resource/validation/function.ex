defmodule Ash.Resource.Validation.Function do
  @moduledoc false

  use Ash.Resource.Validation

  @impl true
  def validate(changeset, [{:fun, {m, f, a}}], context) do
    apply(m, f, [changeset, context | a])
  end

  @impl true
  def validate(changeset, [{:fun, fun}], context) do
    fun.(changeset, context)
  end

  @impl true
  def describe(opts) do
    [
      message: "must pass function %{function}",
      vars: [function: opts[:fun]]
    ]
  end
end
