defmodule Ash.Error.Query.UnsortableAttribute do
  @moduledoc "Used when attempting to sort on a field that cannot be used for sorting"
  use Ash.Error.Exception

  use Splode.Error, fields: [:field], class: :invalid

  def message(%{field: field}) do
    "Cannot sort on #{inspect(field)}"
  end
end
