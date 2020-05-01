defmodule Ash.Error.Changeset do
  def changeset_to_errors(resource, changeset) do
    errors = Ecto.Changeset.traverse_errors(changeset, & &1)

    Enum.flat_map(errors, fn {field, errors} ->
      Enum.map(errors, &to_error(resource, field, &1))
    end)
  end

  defp to_error(_resource, field, {"is invalid", [type: type, validation: :cast]}) do
    Ash.Error.Changes.InvalidValue.exception(field: field, type: type)
  end

  defp to_error(_resource, field, error) do
    Ash.Error.Changes.UnknownError.exception(field: field, error: error)
  end

  def traverse_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
