defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """
  def relate_actor(relationship) do
    {Ash.Resource.Change.RelateActor, relationship: relationship}
  end

  def actor(value), do: {:_actor, value}
end
