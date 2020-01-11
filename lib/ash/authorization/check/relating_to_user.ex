defmodule Ash.Authorization.Check.RelatingToUser do
  use Ash.Authorization.Check, action_types: [:update, :delete]

  @impl true
  def describe(opts) do
    "relating #{opts[:relationship_name]} to the user"
  end

  # TODO: Maybe we should check to see if the pkey of the destination is less fields
  # and as such we'd need to fetch it before we could determine the answer to this check.

  @impl true
  def strict_check(%user_resource{} = user, %{changeset: changeset}, opts) do
    pkey = Ash.primary_key(user_resource)
    pkey_value = Map.take(user, pkey) |> Map.to_list()

    {:ok,
     strict_check_relating_via_attribute?(pkey, pkey_value, opts) ||
       strict_check_relating?(pkey, pkey_value, changeset, opts)}
  end

  def strict_check_relating?(pkey, pkey_value, changeset, opts) do
    case Map.fetch(changeset.__ash_relationships__, opts[:relationship_name]) do
      {:ok, %{add: adding}} ->
        op =
          if opts[:allow_additional?] do
            :any?
          else
            :all?
          end

        found? =
          apply(Enum, op, [
            adding,
            fn relationship_change ->
              Map.take(relationship_change, pkey) == Enum.into(pkey_value, %{})
            end
          ])

        found?

      _ ->
        false
    end
  end

  def strict_check_relating_via_attribute?([pkey_field], pkey_value, changeset, opts) do
    relationship = Ash.relationship(opts[:resource], opts[:relationship_name])

    case relationship do
      %{cardinality: :one, source_field: source_field, destination_field: destination_field} ->
        destination_field == pkey_field &&
          Map.get(pkey_value, pkey_field) == Ecto.Changeset.get_change(changeset, source_field)

      _ ->
        false
    end
  end

  def strict_check_relating_via_attribute?(_, _, _), do: false
end
