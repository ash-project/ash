defmodule Ash.Policy.Check.RelatingToActor do
  @moduledoc "This check is true when the specified relationship is being changed to the current actor."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "relating this.#{opts[:relationship]} to the actor"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
  def match?(nil, _, _), do: false

  def match?(actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    resource = changeset.resource
    relationship = Ash.Resource.Info.relationship(resource, opts[:relationship])

    unless relationship.type == :belongs_to do
      raise "Can only use `belongs_to` relationships in relating_to_actor checks"
    end

    if Ash.Changeset.changing_attribute?(changeset, relationship.source_attribute) do
      Ash.Changeset.get_attribute(changeset, relationship.source_attribute) ==
        Map.get(actor, relationship.destination_attribute)
    else
      case changeset.relationships[relationship.name] do
        [{[input], opts}] ->
          primary_key = Ash.Resource.Info.primary_key(relationship.destination)

          actor_keys = take_keys(actor, primary_key)
          input_keys = take_keys(input, primary_key)

          opts[:on_lookup] == :relate and Enum.all?(actor_keys, & &1) &&
            Enum.all?(input_keys, & &1) and input_keys == actor_keys

        _ ->
          false
      end
    end
  end

  def match?(_, _, _), do: false

  defp take_keys(input, primary_key) do
    Enum.map(primary_key, fn key ->
      Map.get(input, key) || Map.get(input, to_string(key))
    end)
  end
end
