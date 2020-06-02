defmodule Ash.Error.Sort.NoSuchField do
  @moduledoc "Used when attempting to sort on a field that does not exist"
  use Ash.Error

  def_ash_error([:field], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "sort_no_such_field"

    def message(%{field: field, reason: :unsortable_field}) do
      "No such field #{inspect(field)}"
    end

    def description(%{field: field}) do
      "Attempted to sort on a field that does not exist: #{field}"
    end
  end
end
