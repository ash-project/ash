defmodule Ash.Error.Query.InvalidExpression do
  @moduledoc "Used when an invalid expression is used in a filter"
  use Ash.Error

  def_ash_error([:expression, :message], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_expression"

    def message(%{expression: expression, message: message}) do
      "invalid expression: #{inspect(expression)}: #{message}"
    end
  end
end
