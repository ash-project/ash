defmodule Ash.Error.Invalid.ActionRequiresPagination do
  @moduledoc "Used when page option is passed but pagination is not enabled."
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :action], class: :invalid

  def message(%{resource: resource, action: action}) do
    """
    Pagination is not enabled on resource #{inspect(resource)} for the action #{inspect(action)}. Check that you've
    enabled pagination in your action. For example:


    read :#{action.name} do
      pagination offset?: true, keyset?: true, required?: false
    end
    """
  end
end
