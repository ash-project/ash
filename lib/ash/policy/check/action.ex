defmodule Ash.Policy.Check.Action do
  @moduledoc "This check is true when the action name matches the provided action name."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    operator =
      if is_list(options[:action]) do
        "in"
      else
        "=="
      end

    "action #{operator} #{inspect(options[:action])}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(_actor, %{action: %{name: name}}, options) do
    name in List.wrap(options[:action])
  end
end
