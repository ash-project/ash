defmodule Ash.Test.Support.PolicyComplex.User.Changes.AddFriend do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, _, context) do
    Ash.Changeset.after_action(changeset, fn changeset, result ->
      destination_id = Ash.Changeset.get_argument(changeset, :friend_id)

      Ash.Test.Support.PolicyComplex.FriendLink
      |> Ash.Changeset.for_create(:create, %{source_id: result.id, destination_id: destination_id})
      |> changeset.api.create!(Ash.context_to_opts(context))

      {:ok, result}
    end)
  end
end
