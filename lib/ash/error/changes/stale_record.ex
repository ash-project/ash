defmodule Ash.Error.Changes.StaleRecord do
  @moduledoc "Used when a stale record is attempted to be updated or deleted"

  use Splode.Error, fields: [:resource, :filter, :field], class: :invalid

  def message(error) do
    "Attempted to update stale record of #{inspect(error.resource)}#{filter(error.filter)}#{field(error.field)}"
  end

  defp filter(nil), do: ""
  defp filter(%Ash.Filter{expression: nil}), do: ""
  defp filter(%Ash.Filter{expression: expr}), do: " with filter: #{inspect(expr)}"
  defp filter(other), do: " with filter: #{inspect(other)}"

  defp field(nil), do: ""
  defp field(field), do: " with field: #{inspect(field)}"
end
