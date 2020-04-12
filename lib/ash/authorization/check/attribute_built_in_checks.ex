defmodule Ash.Authorization.Check.AttributeBuiltInChecks do
  def setting(opts) do
    {Ash.Authorization.Check.SettingAttribute, Keyword.take(opts, [:to])}
  end

  def logged_in(), do: {Ash.Authorization.Check.LoggedIn, []}
end
