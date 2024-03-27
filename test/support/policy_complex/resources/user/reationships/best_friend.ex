defmodule Ash.Test.Support.PolicyComplex.User.Relationships.BestFriend do
  @moduledoc false
  use Ash.Resource.ManualRelationship
  require Ash.Query

  def load(users, _, %{domain: domain, query: query}) do
    query = Ash.Query.unset(query, [:limit, :offset])
    user_ids = Enum.map(users, & &1.id)

    friend_links =
      Ash.Test.Support.PolicyComplex.FriendLink
      |> Ash.Query.filter(source_id in ^user_ids or destination_id in ^user_ids)
      |> Ash.read!(authorize?: false, domain: domain)

    all_relevant_user_ids =
      friend_links
      |> Enum.flat_map(fn %{source_id: source_id, destination_id: destination_id} ->
        [source_id, destination_id]
      end)
      |> Enum.uniq()

    all_relevant_users =
      query
      |> Ash.Query.filter(id in ^all_relevant_user_ids)
      |> Ash.read!(authorize?: false, domain: domain)
      |> Map.new(&{&1.id, &1})

    {:ok,
     Map.new(users, fn user ->
       best_friend =
         friend_links
         |> Enum.filter(fn %{source_id: source_id, destination_id: destination_id} ->
           source_id == user.id || destination_id == user.id
         end)
         |> Enum.flat_map(fn %{source_id: source_id, destination_id: destination_id} ->
           if source_id == user.id do
             Map.get(all_relevant_users, destination_id)
           else
             Map.get(all_relevant_users, source_id)
           end
           |> List.wrap()
         end)
         |> Enum.uniq_by(& &1.id)
         # obviously a weird heuristic for being someone's best friend 😆
         |> Enum.sort_by(&{String.jaro_distance(user.name, &1.name), &1.name})
         |> Enum.at(0)

       {user.id, best_friend}
     end)}
  end
end
