defmodule Ash.Error.Query.Required do
  @moduledoc "Used when a filter or argument is required in a query"
  use Ash.Error.Exception

  use Splode.Error, fields: [:field, :type, :resource], class: :invalid

  def message(error) do
    "#{error.type} #{error.field} is required"
  end
end
