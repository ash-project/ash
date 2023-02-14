defmodule Ash.Error.Invalid.PageRequiresPagination do
  @moduledoc "Used when page option is passed but pagination is not enabled."
  use Ash.Error.Exception

  def_ash_error([:resource, :action], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "page_requires_pagination"

    def message(%{resource: resource, action: action}) do
      "Pagination is not enabled on the #{inspect action} action on #{inspect resource}."
    end
  end
end
