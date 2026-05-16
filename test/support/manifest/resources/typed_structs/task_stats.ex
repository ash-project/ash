# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TaskStats do
  @moduledoc """
  Test TypedStruct for task statistics.
  TypedStruct fixture for the manifest test suite.
  """
  use Ash.TypedStruct

  typed_struct do
    field(:total_count, :integer, default: 0)
    field(:completed?, :boolean)
    field(:is_urgent?, :boolean, default: false)
    field(:average_duration, :float)
  end
end
