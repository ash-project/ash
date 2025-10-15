# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.VectorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it casts list to Ash.Vector" do
    list = [1.0, 2.0, 3.0]

    assert {:ok, vector} = Ash.Type.cast_input(Ash.Type.Vector, list)
    assert {:ok, ^vector} = Ash.Type.dump_to_native(Ash.Type.Vector, list)
    assert {:ok, ^vector} = Ash.Type.dump_to_native(Ash.Type.Vector, vector)
  end

  test "it casts nil to nil" do
    assert {:ok, nil} = Ash.Type.cast_input(Ash.Type.Vector, nil)
    assert {:ok, nil} = Ash.Type.cast_stored(Ash.Type.Vector, nil)
    assert {:ok, nil} = Ash.Type.dump_to_native(Ash.Type.Vector, nil)
  end
end
