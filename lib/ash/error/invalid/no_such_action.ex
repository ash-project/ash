defmodule Ash.Error.Invalid.NoSuchAction do
  @moduledoc "Used when an action name is provided that doesn't exist"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :action, :type], class: :invalid

  def message(%{resource: resource, action: action, type: type}) do
    "No such action #{inspect(action)} of type #{inspect(type)} for resource #{inspect(resource)}"
  end
end
