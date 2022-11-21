defmodule Ash.Error.Invalid.InvalidPrimaryKey do
  @moduledoc "Used when an invalid primary key is given to an Api's `get`"
  use Ash.Error.Exception

  def_ash_error([:resource, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_primary_key"

    def message(%{resource: _resource, value: value}) do
      # TODO: removed the resource so as not to leak it, but it would be nice to show it internally somehow
      # perhaps an `internal_message` that is only shown on raised exceptions
      "invalid primary key #{inspect(value)} provided"
    end
  end
end
