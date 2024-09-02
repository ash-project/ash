defmodule Ash.Policy.Check.RelatingToActor do
  @moduledoc "This check is true when the specified relationship is being changed to the current actor."
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    "relating this.#{opts[:relationship]} to the actor"
  end

  @impl true
  def filter(nil, _, _), do: false

  def filter(actor, %{changeset: %Ash.Changeset{} = changeset}, opts) do
    resource = changeset.resource
    relationship = Ash.Resource.Info.relationship(resource, opts[:relationship])

    if is_nil(Map.get(actor, relationship.destination_attribute)) do
      false
    else
      unless relationship.type == :belongs_to do
        raise "Can only use `belongs_to` relationships in relating_to_actor checks"
      end

      if Ash.Changeset.changing_attribute?(changeset, relationship.source_attribute) do
        if changeset.action.type == :create do
          actor_value = Map.get(actor, relationship.destination_attribute)

          case Map.fetch(changeset.attributes, relationship.source_attribute) do
            {:ok, ^actor_value} ->
              if Keyword.has_key?(changeset.atomics, relationship.source_attribute) do
                expr(
                  ^atomic_ref(relationship.source_attribute) ==
                    ^Map.get(actor, relationship.destination_attribute)
                )
              else
                true
              end

            _ ->
              false
          end
        else
          if Keyword.has_key?(changeset.atomics, relationship.source_attribute) do
            expr(
              ^atomic_ref(relationship.source_attribute) ==
                ^Map.get(actor, relationship.destination_attribute)
            )
          else
            Ash.Changeset.get_attribute(changeset, relationship.source_attribute) ==
              Map.get(actor, relationship.destination_attribute)
          end
        end
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
  end

  def filter(_, _, _), do: false

  defp take_keys(input, primary_key) do
    Enum.map(primary_key, fn key ->
      Map.get(input, key) || Map.get(input, to_string(key))
    end)
  end
end
