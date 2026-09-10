# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  test "accepts a vector at the 65,535 dimension limit and round-trips it" do
    list = List.duplicate(1.0, 65_535)
    assert {:ok, vector} = Ash.Vector.new(list)
    assert length(Ash.Vector.to_list(vector)) == 65_535
  end

  test "rejects a vector exceeding the 65,535 dimension limit instead of corrupting it" do
    assert {:error, :invalid_vector} = Ash.Vector.new(List.duplicate(1.0, 65_536))

    assert {:error, _} =
             Ash.Type.cast_input(Ash.Type.Vector, List.duplicate(1.0, 65_536))
  end
end
