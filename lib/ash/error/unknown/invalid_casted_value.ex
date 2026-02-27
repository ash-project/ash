defmodule Ash.Error.Unknown.InvalidCastedValue do
  @moduledoc "Used when a value fails to dump to its native (storage) format"

  use Splode.Error, fields: [:type, :value, :constraints, :error], class: :unknown

  def message(%{type: type, value: value, error: error}) do
    "Failed to dump #{inspect(type)} value #{inspect(value)}: #{inspect(error)}"
  end
end
