defmodule Ash.Error.Query.InvalidSortOrder do
  @moduledoc "Used when an invalid sort order is provided"
  use Ash.Error.Exception

  def_ash_error([:order], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_sort_order"

    def message(%{order: order}) do
      "No such sort order #{inspect(order)}"
    end
  end
end
