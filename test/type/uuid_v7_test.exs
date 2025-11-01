# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.UUIDv7Test do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it define the type correctly" do
    assert :uuid = Ash.Type.storage_type(Ash.Type.UUIDv7)
    assert true == Ash.Type.ash_type?(Ash.Type.UUIDv7)
    assert true == Ash.Type.builtin?(Ash.Type.UUIDv7)
    assert Ash.Type.UUIDv7.EctoType = Ash.Type.ecto_type(Ash.Type.UUIDv7)
  end

  # non-exact equality is very expensive and there should be no case where it is necessary
  # If someone needs it, they can define a custom type :)

  # test "it works" do
  #   hex_uuid = "0188aadc-f449-7818-8862-5eff12733f64"
  #   raw_uuid = Ash.UUIDv7.decode(hex_uuid)

  #   assert {:ok, ^hex_uuid} = Ash.Type.cast_input(Ash.Type.UUIDv7, hex_uuid)
  #   assert {:ok, ^hex_uuid} = Ash.Type.cast_input(Ash.Type.UUIDv7, raw_uuid)

  #   assert {:ok, ^hex_uuid} = Ash.Type.cast_stored(Ash.Type.UUIDv7, hex_uuid)
  #   assert {:ok, ^hex_uuid} = Ash.Type.cast_stored(Ash.Type.UUIDv7, raw_uuid)

  #   assert {:ok, ^raw_uuid} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, hex_uuid)
  #   assert {:ok, ^raw_uuid} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, raw_uuid)

  #   assert true == Ash.Type.equal?(Ash.Type.UUIDv7, raw_uuid, hex_uuid)

  #   assert {:ok, ^raw_uuid} = Ash.Type.apply_constraints(Ash.Type.UUIDv7, raw_uuid, [])
  # end

  test "it casts binary UUIDs version 7 to string" do
    uuid_v7 = "01903fa1-2523-7580-a9d6-84620dcbf2ba"

    assert %StreamData{} = Ash.Type.UUIDv7.generator([])

    assert {:ok, binary_uuid_v7} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, uuid_v7)
    assert {:ok, ^uuid_v7} = Ash.Type.cast_input(Ash.Type.UUIDv7, binary_uuid_v7)
  end

  test "it casts v4 UUIDs when match_v4_uuids? is configured true" do
    uuid_v4 = "550e8400-e29b-41d4-a716-446655440000"
    {:ok, binary_uuid_v4} = Ecto.UUID.dump(uuid_v4)
    assert {:ok, ^uuid_v4} = Ash.Type.cast_input(Ash.Type.UUIDv7, binary_uuid_v4)
  end

  test "cast_input/3 accepts valid uuid v7 strings 16 bytes" do
    assert {:ok, "01903fa1-2523-7580-a9d6-84620dcbf2ba"} =
             Ash.Type.cast_input(Ash.Type.UUIDv7, "01903fa1-2523-7580-a9d6-84620dcbf2ba")
  end

  test "it returns an error when casting strings with length of 16 bytes" do
    assert {:error, "is invalid"} = Ash.Type.cast_input(Ash.Type.UUIDv7, "abcdefghabcdefgh")
  end

  test "coerce/2 accepts any 128-bit binary and encodes it" do
    # Create an arbitrary 16-byte (128-bit) binary
    arbitrary_binary = "abcdefghabcdefgh"
    assert byte_size(arbitrary_binary) == 16

    # coerce should accept it and encode it to a UUID string format
    assert {:ok, result} = Ash.Type.coerce(Ash.Type.UUIDv7, arbitrary_binary)
    assert is_binary(result)
    # Should be in UUID format: 8-4-4-4-12 with hyphens
    assert String.length(result) == 36

    assert String.match?(
             result,
             ~r/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/
           )
  end

  test "coerce/2 accepts valid UUIDv7 binary" do
    uuid_v7 = "01903fa1-2523-7580-a9d6-84620dcbf2ba"
    {:ok, binary_uuid_v7} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, uuid_v7)

    assert {:ok, ^uuid_v7} = Ash.Type.coerce(Ash.Type.UUIDv7, binary_uuid_v7)
  end
end
