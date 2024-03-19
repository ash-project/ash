defmodule Ash.Error.Query.UnsortableField do
  @moduledoc "Used when attempting to sort on a field that cannot be used for sorting"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :field, :reason], class: :invalid

  def message(%{field: field, reason: :type, resource: resource}) do
    "#{inspect(field)} is not sortable on #{inspect(resource)}, as its type is not sortable by the data layer"
  end

  def message(%{field: field, resource: resource}) do
    "#{inspect(field)} is not sortable on #{inspect(resource)}"
  end
end
