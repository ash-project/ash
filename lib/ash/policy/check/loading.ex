defmodule Ash.Policy.Check.Loading do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "loading #{opts[:field]}"
  end

  @impl true
  def match?(_actor, %{query: %Ash.Query{} = query}, opts) do
    Ash.Query.loading?(query, opts[:field])
  end

  def match?(_, _, _), do: false
end
