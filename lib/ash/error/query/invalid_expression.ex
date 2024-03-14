defmodule Ash.Error.Query.InvalidExpression do
  @moduledoc "Used when an invalid expression is used in a filter"
  use Ash.Error.Exception

  use Splode.Error, fields: [:expression, :message], class: :invalid

  def splode_message(%{expression: expression, message: message}) do
    "invalid expression: #{inspect(expression)}: #{message}"
  end
end
