defmodule Ash.Error.Page.InvalidKeyset do
  @moduledoc "Used when a value is provided for a keyset that cannot be Base64 decoded."
  use Ash.Error.Exception

  use Splode.Error, fields: [:value, :key], class: :invalid

  def message(%{value: value, key: nil}) do
    "Invalid value provided as a keyset: #{inspect(value)}"
  end

  def message(%{value: value, key: key}) do
    "Invalid value provided as a keyset for #{to_string(key)}: #{inspect(value)}"
  end
end
