# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.IntersectsTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.Intersects

  test "intersects query function" do
    assert {:known, true} = Intersects.evaluate(%{arguments: [[1, 2, 3], [1]]})
    assert {:known, false} = Intersects.evaluate(%{arguments: [[1, 2, 3], [5]]})
    assert {:known, nil} = Intersects.evaluate(%{arguments: [nil, [5]]})
    assert {:known, nil} = Intersects.evaluate(%{arguments: [[1, 2, 3], nil]})
  end
end
