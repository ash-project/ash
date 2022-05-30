defmodule Ash.Error.Page.InvalidKeyset do
  @moduledoc "Used when a value is provided for a keyset that cannot be Base64 decoded."
  use Ash.Error.Exception

  def_ash_error([:value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_keyset"

    def message(%{value: value}) do
      "Invalid value provided as a keyset: #{inspect(value)}"
    end
  end
end
