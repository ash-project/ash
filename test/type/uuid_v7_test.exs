defmodule Ash.Test.Type.UUIDv7Test do
  @moduledoc false
  use ExUnit.Case, async: true

  test "it define the type correctly" do
    assert :uuid = Ash.Type.storage_type(Ash.Type.UUIDv7)
    assert true == Ash.Type.ash_type?(Ash.Type.UUIDv7)
    assert true == Ash.Type.builtin?(Ash.Type.UUIDv7)
    assert Ash.Type.UUIDv7.EctoType = Ash.Type.ecto_type(Ash.Type.UUIDv7)
  end

  test "it works" do
    hex_uuid = "0188aadc-f449-7818-8862-5eff12733f64"
    raw_uuid = Ash.UUIDv7.decode(hex_uuid)

    assert {:ok, ^hex_uuid} = Ash.Type.cast_input(Ash.Type.UUIDv7, hex_uuid)
    assert {:ok, ^hex_uuid} = Ash.Type.cast_input(Ash.Type.UUIDv7, raw_uuid)

    assert {:ok, ^hex_uuid} = Ash.Type.cast_stored(Ash.Type.UUIDv7, hex_uuid)
    assert {:ok, ^hex_uuid} = Ash.Type.cast_stored(Ash.Type.UUIDv7, raw_uuid)

    assert {:ok, ^raw_uuid} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, hex_uuid)
    assert {:ok, ^raw_uuid} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, raw_uuid)

    assert true == Ash.Type.equal?(Ash.Type.UUIDv7, raw_uuid, hex_uuid)

    assert {:ok, ^raw_uuid} = Ash.Type.apply_constraints(Ash.Type.UUIDv7, raw_uuid, [])
  end

  test "it casts binary UUIDs version 7 to string" do
    uuid_v7 = "01903fa1-2523-7580-a9d6-84620dcbf2ba"

    assert %StreamData{} = Ash.Type.UUIDv7.generator([])

    assert {:ok, binary_uuid_v7} = Ash.Type.dump_to_native(Ash.Type.UUIDv7, uuid_v7)
    assert {:ok, ^uuid_v7} = Ash.Type.cast_input(Ash.Type.UUIDv7, binary_uuid_v7)
  end
end
