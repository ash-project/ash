defmodule Ash.Error.InvalidLimit do
  @moduledoc "Used when an invalid limit is provided"
  use Ash.Error

  def_ash_error([:limit], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_limit"

    def message(%{limit: limit}) do
      "#{inspect(limit)} is not a valid limit"
    end
  end
end
