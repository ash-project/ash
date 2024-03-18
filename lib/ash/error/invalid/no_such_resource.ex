defmodule Ash.Error.Invalid.NoSuchResource do
  @moduledoc "Used when a resource or alias is provided that doesn't exist"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :message], class: :invalid

  def message(%{message: message}) when message not in ["", nil], do: message

  def message(%{resource: resource}) do
    "No such resource #{inspect(resource)}"
  end
end
