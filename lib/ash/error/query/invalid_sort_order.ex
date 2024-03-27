defmodule Ash.Error.Query.InvalidSortOrder do
  @moduledoc "Used when an invalid sort order is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:order], class: :invalid

  def message(%{order: order}) do
    "No such sort order #{inspect(order)}"
  end
end
