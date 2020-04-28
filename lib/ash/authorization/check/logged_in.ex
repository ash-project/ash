defmodule Ash.Authorization.Check.LoggedIn do
  use Ash.Authorization.Check, action_types: [:read, :update, :delete, :create], pure?: true

  @impl true
  def describe(_opts) do
    "user is logged in"
  end

  @impl true
  def strict_check(nil, _request, _options), do: {:ok, false}
  def strict_check(_, _request, _options), do: {:ok, true}
end
