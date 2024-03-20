defmodule Ash.Test.Type.VectorTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it casts list to Ash.Vector" do
    list = [1.0, 2.0, 3.0]

    assert {:ok, vector} = Ash.Type.cast_input(Ash.Type.Vector, list)
    assert {:ok, ^vector} = Ash.Type.dump_to_native(Ash.Type.Vector, list)
    assert {:ok, ^vector} = Ash.Type.dump_to_native(Ash.Type.Vector, vector)
  end
end
