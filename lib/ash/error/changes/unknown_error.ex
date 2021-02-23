defmodule Ash.Error.Changes.UnknownError do
  @moduledoc "Used when a change fails for an unknown reason"
  use Ash.Error.Exception

  def_ash_error([:field, :error], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "unknown_change_error"

    def message(%{field: field, error: error}) do
      "Unknown error for change on field #{field}: #{inspect(error)}"
    end
  end
end
