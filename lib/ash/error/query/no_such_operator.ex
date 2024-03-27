defmodule Ash.Error.Query.NoSuchOperator do
  @moduledoc "Used when an operator that doesn't exist is used in a query"
  use Ash.Error.Exception

  use Splode.Error, fields: [:operator], class: :invalid

  def message(error) do
    "No such operator #{error.operator}"
  end
end
