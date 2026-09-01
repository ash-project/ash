# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.DateGrouping do
  @moduledoc """
  Enum used only as a calculation argument type, pinning reachability
  discovery of calculation arguments.
  """
  use Ash.Type.Enum, values: [:day, :week, :month]
end
