defmodule Ash.Policy.Check.Resource do
  @moduledoc "This check is true when the resource matches the provided resource name or names."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    {operator, resource} =
      case options[:resource] do
        [resource] -> {"==", resource}
        resources -> {"in", resources}
      end

    "resource #{operator} #{resource}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(_actor, %{resource: resource}, options) do
    resource in options[:resource]
  end
end
