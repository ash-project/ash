defmodule Ash.Error.Query.UnsortableAttribute do
  @moduledoc "Used when attempting to sort on a field that cannot be used for sorting"
  use Ash.Error.Exception

  def_ash_error([:field], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unsortable_field"

    def message(%{field: field}) do
      "Cannot sort on #{inspect(field)}"
    end
  end
end
