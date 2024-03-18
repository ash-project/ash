defmodule Ash.Error.Query.InvalidLimit do
  @moduledoc "Used when an invalid limit is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:limit], class: :invalid

  def message(%{limit: limit}) do
    "#{inspect(limit)} is not a valid limit"
  end
end
