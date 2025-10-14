# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.TimeUsec do
  @moduledoc """
  Represents a time with `microsecond` precision.

  A builtin type that can be referenced via `:time_usec`
  """
  use Ash.Type.NewType, subtype_of: :time, constraints: [precision: :microsecond]
end
