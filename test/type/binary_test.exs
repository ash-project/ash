# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.BinaryTest do
  use ExUnit.Case, async: true

  @binary_hash :crypto.strong_rand_bytes(32)

  describe "dump_to_embedded" do
    test "base64-encodes binary values" do
      assert {:ok, encoded} = Ash.Type.dump_to_embedded(:binary, @binary_hash, [])
      assert encoded == Base.encode64(@binary_hash)
    end

    test "handles nil" do
      assert {:ok, nil} = Ash.Type.dump_to_embedded(:binary, nil, [])
    end
  end

  describe "cast_from_embedded" do
    test "base64-decodes encoded values" do
      encoded = Base.encode64(@binary_hash)
      assert {:ok, decoded} = Ash.Type.cast_from_embedded(:binary, encoded, [])
      assert decoded == @binary_hash
    end

    test "handles nil" do
      assert {:ok, nil} = Ash.Type.cast_from_embedded(:binary, nil, [])
    end

    test "returns ok with value for non-base64 strings" do
      assert {:ok, "plain text"} = Ash.Type.cast_from_embedded(:binary, "plain text", [])
    end
  end

  describe "dump_to_embedded/cast_from_embedded round-trip" do
    test "preserves binary data" do
      assert {:ok, dumped} = Ash.Type.dump_to_embedded(:binary, @binary_hash, [])
      assert {:ok, restored} = Ash.Type.cast_from_embedded(:binary, dumped, [])
      assert restored == @binary_hash
    end

    test "preserves nil" do
      assert {:ok, nil} = Ash.Type.dump_to_embedded(:binary, nil, [])
      assert {:ok, nil} = Ash.Type.cast_from_embedded(:binary, nil, [])
    end
  end

  describe "array round-trip" do
    test "encodes and decodes each element through cast_from_embedded_array" do
      hashes = [
        :crypto.strong_rand_bytes(16),
        :crypto.strong_rand_bytes(24),
        :crypto.strong_rand_bytes(32)
      ]

      assert {:ok, dumped} = Ash.Type.dump_to_embedded({:array, :binary}, hashes, [])
      assert dumped == Enum.map(hashes, &Base.encode64/1)

      assert {:ok, restored} = Ash.Type.cast_from_embedded({:array, :binary}, dumped, [])
      assert restored == hashes
    end

    test "handles nil" do
      assert {:ok, nil} = Ash.Type.cast_from_embedded({:array, :binary}, nil, [])
    end
  end

  describe "dump_to_native/cast_stored are unchanged" do
    test "dump_to_native returns raw binary" do
      assert {:ok, raw} = Ash.Type.dump_to_native(:binary, @binary_hash, [])
      assert raw == @binary_hash
    end

    test "cast_stored returns raw binary" do
      assert {:ok, raw} = Ash.Type.cast_stored(:binary, @binary_hash, [])
      assert raw == @binary_hash
    end
  end
end
