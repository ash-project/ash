defmodule Ash.Test.Support.PolicyComplex.FriendLink.Changes.SortIds do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, _, _) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      source_id = Ash.Changeset.get_attribute(changeset, :source_id)
      destination_id = Ash.Changeset.get_attribute(changeset, :destination_id)
      [new_source_id, new_destination_id] = Enum.sort([source_id, destination_id])

      if [source_id, destination_id] == [new_source_id, new_destination_id] do
        changeset
      else
        changeset
        |> Ash.Changeset.force_change_attribute(:source_id, new_source_id)
        |> Ash.Changeset.force_change_attribute(:destination_id, new_destination_id)
      end
    end)
  end
end
