# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  test "coerce/2 accepts any 128-bit binary and encodes it" do
    # Create an arbitrary 16-byte (128-bit) binary
    arbitrary_binary = "abcdefghabcdefgh"
    assert byte_size(arbitrary_binary) == 16

    # coerce should accept it and encode it to a UUID string format
    assert {:ok, result} = Ash.Type.coerce(Ash.Type.UUID, arbitrary_binary)
    assert is_binary(result)
    # Should be in UUID format: 8-4-4-4-12 with hyphens
    assert String.length(result) == 36

    assert String.match?(
             result,
             ~r/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/
           )
  end

  test "coerce/2 accepts valid UUID binary" do
    uuid = "a7cec9ba-15de-4c56-99e4-c2abc91a2209"
    {:ok, binary_uuid} = Ash.Type.dump_to_native(Ash.Type.UUID, uuid)

    assert {:ok, ^uuid} = Ash.Type.coerce(Ash.Type.UUID, binary_uuid)
  end
end
