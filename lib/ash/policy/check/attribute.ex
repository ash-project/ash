defmodule Ash.Policy.Check.Attribute do
  @moduledoc "This check is true when a field on the record matches a specific filter."

  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    "record.#{opts[:attribute]} matches #{inspect(opts[:filter])}"
  end

  @impl true
  def filter(opts) do
    [{opts[:attribute], opts[:filter]}]
  end
end
