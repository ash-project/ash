defmodule Ash.Test.Support.PolicyComplex.User.Relationships.Friends do
  @moduledoc false
  use Ash.Resource.ManualRelationship
  require Ash.Query

  def load(users, _, %{api: api, query: query}) do
    user_ids = Enum.map(users, & &1.id)

    friend_links =
      Ash.Test.Support.PolicyComplex.FriendLink
      |> Ash.Query.filter(source_id in ^user_ids or destination_id in ^user_ids)
      |> api.read!(authorize?: false)

    all_relevant_user_ids =
      friend_links
      |> Enum.flat_map(fn %{source_id: source_id, destination_id: destination_id} ->
        [source_id, destination_id]
      end)
      |> Enum.uniq()

    all_relevant_users =
      query
      |> Ash.Query.filter(id in ^all_relevant_user_ids)
      |> api.read!(authorize?: false)
      |> Map.new(&{&1.id, &1})

    {:ok,
     Map.new(users, fn user ->
       friends =
         friend_links
         |> Enum.filter(fn %{source_id: source_id, destination_id: destination_id} ->
           source_id == user.id || destination_id == user.id
         end)
         |> Enum.map(fn %{source_id: source_id, destination_id: destination_id} ->
           if source_id == user.id do
             Map.get(all_relevant_users, destination_id)
           else
             Map.get(all_relevant_users, source_id)
           end
         end)
         |> Enum.uniq()

       {user.id, friends}
     end)}
  end
end
