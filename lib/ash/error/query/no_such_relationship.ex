defmodule Ash.Error.Query.NoSuchRelationship do
  @moduledoc "Used when an relationship that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:resource, :relationship], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_relationship"

    def message(error) do
      "No such relationship #{error.relationship} for resource #{inspect(error.resource)}"
    end
  end
end
