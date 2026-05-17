# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Types.EmailString do
  @moduledoc """
  A NewType wrapping `:string`, used by manifest tests to verify operator
  resolution falls through to the underlying base type.
  """
  use Ash.Type.NewType, subtype_of: :string
end
