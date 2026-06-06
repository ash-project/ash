# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.ByteSizeTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  alias Ash.Query.Function.ByteSize

  test "evaluates the byte size of strings" do
    assert {:known, 3} = ByteSize.evaluate(%{arguments: ["yes"]})
    assert {:known, 4} = ByteSize.evaluate(%{arguments: ["🔥"]})
  end

  test "evaluates the byte size of ci strings" do
    assert {:known, 4} = ByteSize.evaluate(%{arguments: [%Ash.CiString{string: "🔥"}]})
  end

  test "can be used in expressions" do
    assert eval!(expr(byte_size("🔥"))) == 4
  end
end
