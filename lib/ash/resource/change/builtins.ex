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

  @doc """
  Clears a change off of the changeset before the action runs.

  Useful if a change is only used in validations but shouldn't ultimately be written to the data layer
  """
  def prevent_change(field) do
    {Ash.Resource.Change.PreventChange, field: field}
  end
end
