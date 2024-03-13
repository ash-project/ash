defmodule Ash.Error.Query.NoSuchField do
  @moduledoc "Used when a field(attrbute, calculation, aggregate or relationship) that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:resource, :field], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_field"

    def message(error) do
      "No such field #{error.field} for resource #{inspect(error.resource)}"
    end
  end
end
