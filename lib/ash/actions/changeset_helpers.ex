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

      _, {:error, error} ->
        {:error, error}
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

  def belongs_to_assoc_update(
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
        before_change(changeset, fn changeset ->
          case api.get(destination, filter, %{authorize?: authorize?, user: user}) do
            {:ok, record} ->
              changeset
              |> Ecto.Changeset.put_change(source_field, Map.get(record, destination_field))
              |> after_change(fn _changeset, result ->
                {:ok, Map.put(result, rel_name, record)}
              end)

            {:error, error} ->
              {:error, error}
          end
        end)
    end
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

          with {:ok, record} <-
                 api.get(destination, filter, %{authorize?: authorize?, user: user}),
               {:ok, updated_record} <-
                 api.update(record, %{attributes: %{destination_field => value}}) do
            {:ok, Map.put(result, rel_name, updated_record)}
          end
        end)
    end
  end
end
