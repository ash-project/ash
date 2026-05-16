# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.Percentage do
  @moduledoc """
  A NewType wrapping :float .
  Demonstrates that NewTypes are resolved as type_refs.
  """
  use Ash.Type.NewType, subtype_of: :float
end
