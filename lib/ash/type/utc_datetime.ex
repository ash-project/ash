defmodule Ash.Type.UtcDatetime do
  @moduledoc """
  Represents a utc datetime. A wrapper around `:datetime` for backwards compatibility.
  """
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :second]
end
