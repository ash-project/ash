defmodule Ash.Authorization.Check.RelationshipBuiltInChecks do
  @moduledoc "The relationship specific authorization checks built into ash"

  def relating_to_user(opts \\ []) do
    {Ash.Authorization.Check.RelatingToUser, opts}
  end

  def relationship_set() do
    {Ash.Authorization.Check.RelationshipSet, []}
  end

  def logged_in(), do: {Ash.Authorization.Check.LoggedIn, []}
end
