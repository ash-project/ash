defmodule Ash.Type.UtcDatetimeUsec do
  @moduledoc """
  Represents a utc datetime with `nanosecond` precision. A wrapper around `:datetime` for backwards compatibility.
  """
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :microsecond]
end
