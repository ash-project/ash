defmodule Ash.Test.Type.UUIDTest do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it casts binary UUIDs to string" do
    uuid = "a7cec9ba-15de-4c56-99e4-c2abc91a2209"

    assert {:ok, binary_uuid} = Ash.Type.dump_to_native(Ash.Type.UUID, uuid)
    assert {:ok, ^uuid} = Ash.Type.cast_input(Ash.Type.UUID, binary_uuid)
  end

  test "cast_input/3 accepts valid uuid strings 16 bytes" do
    assert {:ok, "a7cec9ba-15de-4c56-99e4-c2abc91a2209"} =
             Ash.Type.cast_input(Ash.Type.UUID, "a7cec9ba-15de-4c56-99e4-c2abc91a2209")
  end

  test "it returns an error when casting strings with length of 16 bytes" do
    assert {:error, "is invalid"} = Ash.Type.cast_input(Ash.Type.UUID, "abcdefghabcdefgh")
  end
end
