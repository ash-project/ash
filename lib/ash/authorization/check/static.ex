defmodule Ash.Authorization.Check.Static do
  use Ash.Authorization.Check

  @impl true
  def describe(options) do
    "always #{inspect(options[:result])}"
  end

  @impl true
  def strict_check(_user, _request, options) do
    {:ok, options[:result]}
  end
end
