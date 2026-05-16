# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.Status do
  @moduledoc """
  Todo status enumeration.
  """
  use Ash.Type.Enum, values: [:pending, :ongoing, :finished, :cancelled]
end
