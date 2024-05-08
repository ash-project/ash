defmodule Ash.ForbiddenField do
  @moduledoc "Represents a field that was hidden due to authorization rules."
  @derive {Inspect, only: [:field, :type]}
  # original_value is unfortunately required for embedded attributes, although hidden
  # embedded resources have to be able to be written back, even if the user can't see
  # all of the fields, so original values must be retained
  defstruct [:field, :type, :original_value]
end
