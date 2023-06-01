defmodule Ash.Resource.Validation.Confirm do
  @moduledoc false
  use Ash.Resource.Validation
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

  def validate(changeset, opts) do
    confirmation_value =
      Changeset.get_argument(changeset, opts[:confirmation]) ||
        Changeset.get_attribute(changeset, opts[:confirmation])

    value =
      Changeset.get_argument(changeset, opts[:field]) ||
        Changeset.get_attribute(changeset, opts[:field])

    if Comp.equal?(confirmation_value, value) do
      :ok
    else
      {:error,
       InvalidAttribute.exception(
         field: opts[:confirmation],
         value: confirmation_value,
         message: "confirmation did not match value"
       )}
    end
  end
end
