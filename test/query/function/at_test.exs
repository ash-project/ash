# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.AtTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  test "indexing works" do
    assert 1 = eval!(expr(at([1, 2, 3], 0)))
  end
end
