defmodule Ash.Resource.Change.Builtins do
  @moduledoc """
  Built in changes that are available to all resources

  The functions in this module are imported by default in the actions section.
  """

  @doc "Relates the actor to the data being changed, as the provided relationship."
  def relate_actor(relationship) do
    {Ash.Resource.Change.RelateActor, relationship: relationship}
  end

  @doc """
  Sets the attribute to the value provided. If a zero argument function is provided, it is called to determine the value.
  """
  def set_attribute(attribute, value) do
    {Ash.Resource.Change.SetAttribute, attribute: attribute, value: value}
  end

  @doc "A helper for builting filter templates"
  def actor(value), do: {:_actor, value}

  @doc "A helper to confirm the value of one field against another field, or an argument"
  def confirm(field, confirmation) do
    {Ash.Resource.Change.Confirm, [field: field, confirmation: confirmation]}
  end
end
