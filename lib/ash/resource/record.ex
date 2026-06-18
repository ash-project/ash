# SPDX-FileCopyrightText: 2026 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Record do
  @moduledoc """
  In Ash, a record is an instance of a resource.

  This module acts as a way to refer to the type via t:t()
  and will also act as a place for shared functionality for
  instances of resouces in the future.
  """
  @type t :: struct()
end
