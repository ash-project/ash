defmodule Ash.Error.Invalid.PageRequiresPagination do
  @moduledoc "Used when page option is passed but pagination is not enabled."
  use Ash.Error.Exception

  def_ash_error([:resource, :action], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "page_requires_pagination"

    def message(%{resource: resource, action: action}) do
      "Pagination is not enabled on resource #{inspect(resource)} for the action #{inspect(action)}. Check that you've set pagination to `true` in your action and also that you have set a page in your opts like this MyApp.MyAPI.read!(MyResource, page: [limit: 1])"
    end
  end
end
