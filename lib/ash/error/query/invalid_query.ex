defmodule Ash.Error.Query.InvalidQuery do
  @moduledoc """
  A generic error that can be used to add an error to a query for a specific field
  """
  use Ash.Error.Exception

  use Splode.Error, fields: [:field, :message], class: :invalid

  def message(error) do
    error.message
  end
end
