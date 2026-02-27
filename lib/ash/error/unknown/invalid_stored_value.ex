defmodule Ash.Error.Unknown.InvalidStoredValue do
  @moduledoc "Used when a stored value fails to be cast from the data layer"

  use Splode.Error, fields: [:type, :value, :constraints, :error], class: :unknown

  def message(%{type: type, value: value, error: error}) do
    "Failed to load #{inspect(type)} value #{inspect(value)}: #{inspect(error)}"
  end
end
