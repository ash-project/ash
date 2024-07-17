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
    assert {:ok, nil} = Ash.Type.Vector.cast_input(nil, nil)
    assert {:ok, nil} = Ash.Type.Vector.cast_stored(nil, nil)
    assert {:ok, nil} = Ash.Type.Vector.dump_to_native(nil, nil)
  end
end
