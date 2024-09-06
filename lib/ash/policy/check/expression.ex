defmodule Ash.Policy.Check.Expression do
  @moduledoc "The check module used for `expr`s in policies"
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    inspect(opts[:expr])
  end

  @impl true
  def filter(_, _, opts) do
    opts[:expr]
  end

  @impl true
  def prefer_expanded_description?, do: true
end
