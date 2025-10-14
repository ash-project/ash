# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.UUIDv7Test do
  @moduledoc false
  use ExUnit.Case, async: true

  test "generate/1 is working" do
    uuid = Ash.UUIDv7.generate()

    assert <<_::64, ?-, _::32, ?-, "7", _::24, ?-, _::32, ?-, _::96>> = uuid
  end

  test "generate/1 is ordered" do
    uuids =
      for _ <- 1..100 do
        uuid = Ash.UUIDv7.generate()
        # only guaranteed sorted if >= 1 nanosecond apart
        # can't sleep for one nanosecond AFAIK, so sleep for 1 ms
        :timer.sleep(3)
        uuid
      end

    assert uuids == Enum.sort(uuids)
  end

  test "bingenerate/1 is ordered" do
    uuids =
      for _ <- 1..100 do
        uuid = Ash.UUIDv7.bingenerate()
        # only guaranteed sorted if >= 1 nanosecond apart
        # can't sleep for one nanosecond AFAIK, so sleep for 1 ms
        :timer.sleep(3)
        uuid
      end

    assert uuids == Enum.sort(uuids)
  end

  test "encode/1 is working" do
    hex_uuid = Ash.UUIDv7.generate()
    raw_uuid = Ash.UUIDv7.bingenerate()

    encoded_hex_uuid = Ash.UUIDv7.encode(hex_uuid)
    encoded_raw_uuid = Ash.UUIDv7.encode(raw_uuid)

    assert is_binary(encoded_hex_uuid)
    assert Ash.UUIDv7.decode(hex_uuid) == Ash.UUIDv7.decode(encoded_hex_uuid)

    assert is_binary(encoded_raw_uuid)
    assert raw_uuid == Ash.UUIDv7.decode(encoded_raw_uuid)

    assert :error = Ash.UUIDv7.encode("error")
  end

  test "decode/1 is working" do
    hex_uuid = Ash.UUIDv7.generate()
    raw_uuid = Ash.UUIDv7.bingenerate()

    decoded_hex_uuid = Ash.UUIDv7.decode(hex_uuid)
    decoded_raw_uuid = Ash.UUIDv7.decode(raw_uuid)

    assert is_binary(decoded_hex_uuid)
    assert hex_uuid == Ash.UUIDv7.encode(decoded_hex_uuid)

    assert is_binary(decoded_raw_uuid)
    assert Ash.UUIDv7.encode(raw_uuid) == Ash.UUIDv7.encode(decoded_raw_uuid)

    assert :error == Ash.UUIDv7.decode("error")
  end
end
