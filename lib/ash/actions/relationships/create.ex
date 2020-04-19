# defmodule Ash.Actions.Relationships.Create do
#   alias Ash.Actions.Relationships.Change

#   def changeset(changeset, api, relationships) do
#     relationship_changes = relationship_changes(relationships)

#     changeset
#   end

#   defp relationship_changes(relationships) do
#     Enum.into(relationships, %{}, fn {key, value} ->
#       {key, Change.from(value, :create)}
#     end)
#   end

#   # def changeset(changeset, api, relationships) do
#   #   if relationships == %{} do
#   #     changeset
#   #   else
#   #     dependencies = Map.get(changeset, :__changes_depend_on__, [])

#   #     Ash.Engine.Request.UnresolvedField.field(dependencies, fn data ->
#   #       new_changeset =
#   #         data
#   #         |> Map.get(:relationships, %{})
#   #         |> Enum.reduce(changeset, fn {relationship, relationship_data}, changeset ->
#   #           relationship = Ash.relationship(changeset.data.__struct__, relationship)

#   #           relationship_data =
#   #             relationship_data
#   #             |> Enum.into(%{}, fn {key, value} ->
#   #               {key, value.data}
#   #             end)
#   #             |> Map.put_new(:current, [])

#   #           add_relationship_to_changeset(changeset, api, relationship, relationship_data)
#   #         end)

#   #       {:ok, new_changeset}
#   #     end)
#   #   end
#   # end
# end
