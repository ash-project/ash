defmodule Ash.Error.Invalid.PaginationRequired do
  @moduledoc "Used when `page: false` is provided but pagination is required"
  use Ash.Error

  def_ash_error([], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "pagination_required"

    def class(_), do: :invalid

    def message(_) do
      "Pagination is required"
    end

    def stacktrace(_), do: nil
  end
end
