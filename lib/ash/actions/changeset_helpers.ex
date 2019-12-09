defmodule Ash.Actions.ChangesetHelpers do
  alias Ash.Actions.Filter

  def before_change(changeset, func) do
    Map.update(changeset, :__before_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  def after_change(changeset, func) do
    Map.update(changeset, :__after_ash_changes__, [func], fn funcs ->
      [func | funcs]
    end)
  end

  def run_before_changes(%{__before_ash_changes__: hooks} = changeset) do
    Enum.reduce(hooks, changeset, fn
      hook, %Ecto.Changeset{valid?: true} = changeset ->
        case hook.(changeset) do
          :ok -> changeset
          {:ok, changeset} -> changeset
          %Ecto.Changeset{} = changeset -> changeset
        end

      _, %Ecto.Changeset{} = changeset ->
        changeset
    end)
  end

  def run_before_changes(changeset), do: changeset

  def run_after_changes(%{__after_ash_changes__: hooks} = changeset, result) do
    Enum.reduce(hooks, {:ok, result}, fn
      hook, {:ok, result} ->
        case hook.(changeset, result) do
          {:ok, result} -> {:ok, result}
          :ok -> {:ok, result}
          {:error, error} -> {:error, error}
        end

      _, {:error, error} ->
        {:error, error}
    end)
  end

  def run_after_changes(_changeset, result) do
    {:ok, result}
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
        after_change(changeset, fn _changeset, result ->
          value = Map.get(result, source_field)

          get_and_update(api, destination, filter, authorize?, user, %{destination_field => value})
        end)
    end
  end

  defp get_and_update(api, destination, filter, authorize?, user, attribute_updates) do
    with {:ok, record} <-
           api.get(destination, filter, %{authorize?: authorize?, user: user}),
         {:ok, _} <- api.update(record, %{attributes: attribute_updates}) do
      :ok
    end
  end
end
