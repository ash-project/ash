defmodule Ash.Policy.Check.Matches do
  @moduledoc "This check is true when the specified function returns true"
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    options[:description]
  end

  @impl true
  def match?(actor, request, options) do
    options[:func].(actor, request)
  end
end
