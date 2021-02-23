defmodule Ash.Error.Query.NoSuchFunction do
  @moduledoc "Used when an function that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:name], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_function"

    def message(error) do
      "No such function #{error.name}"
    end
  end
end
