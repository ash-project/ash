defmodule Ash.Duration do
  @moduledoc """
  A duration struct for Ash.
  """

  defstruct [:year, :month, :hour, :minute, :microsecond]

  @type t :: %__MODULE__{
          year: integer(),
          month: integer(),
          hour: integer(),
          minute: integer(),
          microsecond: {integer(), integer()}
        }
end
