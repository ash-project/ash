defmodule Ash.Error.Invalid.PaginationRequired do
  @moduledoc "Used when `page: false` is provided but pagination is required"
  use Ash.Error.Exception

  def_ash_error([], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "pagination_required"

    def message(_) do
      "Pagination is required"
    end
  end
end
