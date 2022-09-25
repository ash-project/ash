defmodule Ash.Error.Query.NoSuchFunction do
  @moduledoc "Used when an function that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:name, :arity], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_function"

    def message(error) do
      if error.arity do
        "No such function #{error.name}/#{error.arity}"
      else
        "No such function #{error.name}"
      end
    end
  end
end
