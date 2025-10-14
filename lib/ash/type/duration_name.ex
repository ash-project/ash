# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.DurationName do
  @values ~w(year month week day hour minute second millisecond microsecond)a
  @moduledoc """
  An interval of time, primarily meant to be used in expression functions

  Valid intervals are (as strings or atoms): #{inspect(@values)}
  """
  use Ash.Type.Enum, values: @values
end
