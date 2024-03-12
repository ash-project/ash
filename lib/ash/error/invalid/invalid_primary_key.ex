defmodule Ash.Error.Invalid.InvalidPrimaryKey do
  @moduledoc "Used when an invalid primary key is given to `Ash.get/2`"
  use Ash.Error.Exception

  def_ash_error([:resource, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_primary_key"

    def message(%{resource: _resource, value: value}) do
      "invalid primary key #{inspect(value)} provided"
    end
  end
end
