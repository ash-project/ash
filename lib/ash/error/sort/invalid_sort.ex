defmodule Ash.Error.Sort.UnsortableField do
  use Ash.Error

  def_ash_error([:field], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "unsortable_field"

    def message(%{field: field}) do
      "Cannot sort on #{inspect(field)}"
    end

    def description(%{field: field}) do
      "Attempted to sort on a field that cannot be used for sorts: #{field}"
    end
  end
end
