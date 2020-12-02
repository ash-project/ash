defmodule Ash.Resource.Change.Confirm do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidAttribute

  def init(opts) do
    case opts[:field] do
      nil ->
        {:error, "Field is required"}

      field when is_atom(field) ->
        case opts[:confirmation] do
          nil ->
            {:error, "Confirmation is required"}

          confirmation when is_atom(confirmation) ->
            {:ok, [confirmation: confirmation, field: field]}

          confirmation ->
            {:error, "Expected an atom for confirmation, got: #{inspect(confirmation)}"}
        end

      field ->
        {:error, "Expected an atom for field, got: #{inspect(field)}"}
    end
  end

  def change(changeset, opts, _) do
    confirmation_value =
      Map.get(changeset.arguments, opts[:confirmation]) ||
        Ash.Changeset.get_attribute(changeset, opts[:value])

    if confirmation_value == Ash.Changeset.get_attribute(changeset, opts[:field]) do
      changeset
    else
      Changeset.add_error(
        changeset,
        InvalidAttribute.exception(
          field: opts[:field],
          message: "Value did not match confirmation"
        )
      )
    end
  end
end
