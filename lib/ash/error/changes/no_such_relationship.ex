defmodule Ash.Error.Changes.NoSuchRelationship do
  @moduledoc "Used when a change is provided for an relationship that does not exist"
  use Ash.Error

  def_ash_error([:resource, :name], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_relationship"

    def message(error) do
      "No such relationship #{error.name} for resource #{inspect(error.resource)}"
    end
  end
end
