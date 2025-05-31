defmodule Ash.Error.Invalid.PaginationRequired do
  @moduledoc "Used when `page: false` is provided but pagination is required"

  use Splode.Error, fields: [], class: :invalid

  def message(_) do
    "Pagination is required"
  end
end
