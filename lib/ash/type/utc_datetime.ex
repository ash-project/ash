# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.UtcDatetime do
  @moduledoc """
  Represents a utc datetime, with 'second' precision. A wrapper around `:datetime` for backwards compatibility.

  A builtin type that can be referenced via `:utc_datetime`
  """
  # 4.0 deprecate this type
  use Ash.Type.NewType, subtype_of: :datetime, constraints: [precision: :second]
end
