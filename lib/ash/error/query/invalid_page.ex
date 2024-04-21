defmodule Ash.Error.Query.InvalidPage do
  @moduledoc "Used when an invalid page option is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:page], class: :invalid

  def message(%{page: page}) do
    "#{inspect(page)} is not a valid page option"
  end
end
