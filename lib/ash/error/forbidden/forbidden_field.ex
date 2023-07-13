defmodule Ash.Error.Forbidden.ForbiddenField do
  @moduledoc "Raised in cases where access to a specific field was prevented"

  require Logger
  use Ash.Error.Exception

  def_ash_error([:resource, :field], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def message(error) do
      "Forbidden: cannot access #{error.field} on #{inspect(error.resource)}"
    end

    def code(_), do: "forbidden_field"
  end
end
