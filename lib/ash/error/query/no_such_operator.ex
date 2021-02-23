defmodule Ash.Error.Query.NoSuchOperator do
  @moduledoc "Used when an operator that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:name], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_operator"

    def message(error) do
      "No such operator #{error.name}"
    end
  end
end
