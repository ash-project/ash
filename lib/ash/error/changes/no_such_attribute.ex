defmodule Ash.Error.Changes.NoSuchAttribute do
  @moduledoc "Used when a change is provided for an attribute that does not exist"
  use Ash.Error.Exception

  def_ash_error([:resource, :name], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_attribute"

    def message(error) do
      "No such attribute #{error.name} for resource #{inspect(error.resource)}"
    end
  end
end
