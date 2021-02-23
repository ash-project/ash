defmodule Ash.Error.Query.NoSuchRelationship do
  @moduledoc "Used when an relationship that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:resource, :name], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_relationship"

    def message(error) do
      "No such relationship #{error.name} for resource #{inspect(error.resource)}"
    end
  end
end
