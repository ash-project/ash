defmodule Ash.Error.Changes.InvalidValue do
  use Ash.Error

  def_ash_error([:field, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_change_value"

    def message(%{field: field, type: type}) do
      "Invalid value provided for #{field} of type #{inspect(type)}"
    end

    def description(%{field: field, type: type}) do
      "Invalid value provided for #{field} of type #{inspect(type)}"
    end
  end
end
