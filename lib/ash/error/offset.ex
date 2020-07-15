defmodule Ash.Error.InvalidOffset do
  @moduledoc "Used when an invalid offset is provided"
  use Ash.Error

  def_ash_error([:offset], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_offset"

    def message(%{offset: offset}) do
      "#{inspect(offset)} is not a valid offset"
    end
  end
end
