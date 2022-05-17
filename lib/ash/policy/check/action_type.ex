defmodule Ash.Policy.Check.ActionType do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "action.type == #{inspect(options[:type])}"
  end

  @impl true
  def match?(_actor, %{action: %{type: type}}, options) do
    type == options[:type]
  end
end
