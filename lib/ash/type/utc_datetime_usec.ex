defmodule Ash.Type.UtcDatetimeUsec do
  @moduledoc """
  Represents a utc datetime with `microsecond` precision. A wrapper around `:datetime` for backwards compatibility.

  A builtin type that can be referenced via `:utc_datetime_usec`
  """
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :microsecond]
end
