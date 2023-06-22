defmodule Ash.ForbiddenField do
  @moduledoc "Represents a field that was hidden due to authorization rules."
  defstruct [:field, :type]
end
