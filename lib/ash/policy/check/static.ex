defmodule Ash.Policy.Check.Static do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "always #{inspect(options[:result])}"
  end

  @impl true
  def match?(_actor, _request, options) do
    options[:result]
  end
end
