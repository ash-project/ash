defmodule Ash.Actions.ChangesetHelpers do
  alias Ash.Actions.Filter

  def before_changes(changeset, func) do
    Map.update(changeset, :__before_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  def after_changes(changeset, func) do
    Map.update(changeset, :__after_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  def has_one_assoc_update(
        %{__ash_api__: api} = changeset,
        %{
          destination: destination,
          destination_field: destination_field,
          source_field: source_field,
          name: rel_name
        },
        identifier,
        authorize?,
        user
      ) do
    case Filter.value_to_primary_key_filter(destination, identifier) do
      {:error, _error} ->
        Ecto.Changeset.add_error(changeset, rel_name, "Invalid primary key supplied")

      {:ok, filter} ->
        after_changes(changeset, fn _changeset, result ->
          value = Map.get(result, source_field)

          get_and_update(api, destination, filter, authorize?, user, %{destination_field => value})
        end)
    end
  end

  defp get_and_update(api, destination, filter, authorize?, user, attribute_updates) do
    case api.get(destination, %{filter: filter, authorize?: authorize?, user: user}) do
      {:ok, record} ->
        api.update(record, %{
          attributes: attribute_updates
        })

      {:error, error} ->
        {:error, error}
    end
  end
end
