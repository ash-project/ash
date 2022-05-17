defmodule Ash.Policy.Check.Action do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "action == #{inspect(options[:action])}"
  end

  @impl true
  def match?(_actor, %{action: %{name: name}}, options) do
    name == options[:action]
  end
end
