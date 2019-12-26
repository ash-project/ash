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
    [decision: Map.get(user, options[:field]) == options[:value]]
  end
end
