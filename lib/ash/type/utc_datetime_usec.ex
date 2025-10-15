# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.UtcDatetimeUsec do
  @moduledoc """
  Represents a utc datetime with `microsecond` precision. A wrapper around `:datetime` for backwards compatibility.

  A builtin type that can be referenced via `:utc_datetime_usec`
  """
  # 4.0 deprecate this type
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :microsecond]
end
