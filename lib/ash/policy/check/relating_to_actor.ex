defmodule Ash.Policy.Check.RelatingToActor do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "relating this.#{opts[:relationship]} to the actor"
  end

  @impl true
  def match?(nil, _, _), do: false

  def match?(actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    resource = changeset.resource
    relationship = Ash.Resource.Info.relationship(resource, opts[:relationship])

    unless relationship.type == :belongs_to do
      raise "Can only use `belongs_to` relationships in relating_to_actor checks"
    end

    if Ash.Changeset.changing_attribute?(changeset, relationship.source_field) do
      Ash.Changeset.get_attribute(changeset, relationship.source_field) ==
        Map.get(actor, relationship.destination_field)
    else
      false
    end
  end

  def match?(_, _, _), do: false
end
