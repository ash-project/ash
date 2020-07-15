defmodule Ash.Error.Framework do
  @moduledoc "Used when an unknown/generic framework error occurs"
  use Ash.Error

  def_ash_error([:errors], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "framework"

    def message(error) do
      Ash.Error.error_messages(error.errors)
    end
  end
end
