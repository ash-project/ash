defmodule Ash.Error.Query.InvalidOffset do
  @moduledoc "Used when an invalid offset is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:offset], class: :invalid

  def message(%{offset: offset}) do
    "#{inspect(offset)} is not a valid offset"
  end
end
