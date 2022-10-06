defmodule Ash.Policy.Check.ActionType do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "action.type == #{inspect(options[:type])}"
  end

  @impl true
  def match?(_actor, %{action: %{type: type}}, options) do
    case List.wrap(options[:type]) do
      [configured_type] ->
        type == configured_type

      types ->
        Enum.any?(types, &(&1 == type))
    end
  end
end
