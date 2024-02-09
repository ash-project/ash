defmodule Ash.Policy.Check.Static do
  @moduledoc "This check is always the result provided"
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    "always #{inspect(options[:result])}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(_actor, _request, options) do
    options[:result]
  end
end
