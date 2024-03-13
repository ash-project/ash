defmodule Ash.Error.Query.NoSuchAttribute do
  @moduledoc "Used when an attribute that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:resource, :attribute], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_attribute"

    def message(error) do
      "No such attribute #{error.attribute} for resource #{inspect(error.resource)}"
    end
  end
end
