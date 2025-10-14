# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyComplex.User.Changes.AddFriend do
  @moduledoc false
  use Ash.Resource.Change

  def change(changeset, _, context) do
    Ash.Changeset.after_action(changeset, fn changeset, result ->
      destination_id = Ash.Changeset.get_argument(changeset, :friend_id)

      Ash.Test.Support.PolicyComplex.FriendLink
      |> Ash.Changeset.for_create(
        :create,
        %{source_id: result.id, destination_id: destination_id},
        domain: changeset.domain
      )
      |> Ash.create!(Ash.Context.to_opts(context))

      {:ok, result}
    end)
  end

  def atomic(_, _, _), do: :ok

  def after_batch(changesets_and_results, _opts, context) do
    Enum.each(changesets_and_results, fn {changeset, result} ->
      destination_id = Ash.Changeset.get_argument(changeset, :friend_id)

      Ash.Test.Support.PolicyComplex.FriendLink
      |> Ash.Changeset.for_create(
        :create,
        %{source_id: result.id, destination_id: destination_id},
        domain: changeset.domain
      )
      |> Ash.create!(Ash.Context.to_opts(context))
    end)

    :ok
  end
end
