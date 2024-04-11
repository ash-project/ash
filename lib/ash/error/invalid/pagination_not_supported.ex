defmodule Ash.Error.Invalid.PaginationNotSupported do
  @moduledoc "Used when page option is passed but pagination is not enabled."
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :action], class: :invalid

  def message(%{resource: resource, action: action}) do
    """
    Pagination is not supported for #{inspect(resource)}.#{action}, but
    pagination parameters were supplied.
    """
  end
end
