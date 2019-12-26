defmodule Ash.Authorization.Check.Static do
  use Ash.Authorization.Check

  def always() do
    {__MODULE__, [result: true]}
  end

  def never() do
    {__MODULE__, [result: false]}
  end

  @impl true
  def describe(options) do
    "always #{inspect(options[:result])}"
  end

  @impl true
  def strict_check(_user, _request, options) do
    {:ok, options[:result]}
  end
end
