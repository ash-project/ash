defmodule Ash.Authorization.Check.UserAttribute do
  use Ash.Authorization.Check

  def user_attribute(field, value) do
    {__MODULE__, field: field, value: value}
  end

  @impl true
  def describe(opts) do
    "user.#{opts[:field]} == #{inspect(opts[:value])}"
  end

  @impl true
  def strict_check(user, _request, options) do
    {:ok, Map.fetch(user, options[:field]) == {:ok, options[:value]}}
  end
end
