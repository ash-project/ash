defmodule Ash.Type.UtcDatetime do
  @moduledoc """
  Represents a utc datetime, with 'second' precision. A wrapper around `:datetime` for backwards compatibility.

  A builtin type that can be referenced via `:utc_datetime`
  """
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :second]
end
