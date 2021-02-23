defmodule Ash.Error.Invalid.InvalidPrimaryKey do
  @moduledoc "Used when an invalid primary key is given to an Api's `get`"
  use Ash.Error.Exception

  def_ash_error([:resource, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_primary_key"

    def message(%{resource: resource, value: value}) do
      "invalid primary key #{inspect(value)} provided for resource #{inspect(resource)}"
    end
  end
end
