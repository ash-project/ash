defmodule Ash.Error.Changes.StaleRecord do
  @moduledoc "Used when a stale record is attempted to be updated or deleted"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :filter], class: :invalid

  def message(error) do
    "Attempted to update stale record of #{inspect(error.resource)}#{filter(error.filter)}"
  end

  defp filter(nil), do: ""
  defp filter(%Ash.Filter{expression: nil}), do: ""
  defp filter(%Ash.Filter{expression: expr}), do: " with filter: #{inspect(expr)}"
  defp filter(other), do: " with filter: #{inspect(other)}"
end
