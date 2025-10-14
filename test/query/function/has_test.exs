# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.HasTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.Has

  test "has query function" do
    assert {:known, true} = Has.evaluate(%{arguments: [[1, 2, 3], 1]})
    assert {:known, false} = Has.evaluate(%{arguments: [[1, 2, 3], 5]})
    assert {:known, nil} = Has.evaluate(%{arguments: [nil, 5]})
  end
end
