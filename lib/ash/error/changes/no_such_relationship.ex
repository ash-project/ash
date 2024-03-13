defmodule Ash.Error.Changes.NoSuchRelationship do
  @moduledoc "Used when a change is provided for an relationship that does not exist"
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
