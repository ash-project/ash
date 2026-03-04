# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  test "generate/1 with a DateTime embeds the correct timestamp" do
    dt = ~U[2024-04-11 00:00:00Z]
    expected_ms = DateTime.to_unix(dt, :millisecond)

    uuid = Ash.UUIDv7.generate(dt)
    assert Ash.UUIDv7.extract_timestamp(uuid) == expected_ms
  end

  test "generate/1 with different DateTimes produces correctly ordered UUIDs" do
    dt1 = ~U[2023-01-01 00:00:00Z]
    dt2 = ~U[2024-01-01 00:00:00Z]
    dt3 = ~U[2025-01-01 00:00:00Z]

    uuids = [Ash.UUIDv7.generate(dt1), Ash.UUIDv7.generate(dt2), Ash.UUIDv7.generate(dt3)]
    assert uuids == Enum.sort(uuids)
  end

  test "bingenerate/1 with a DateTime embeds the correct timestamp" do
    dt = ~U[2024-04-11 00:00:00Z]
    expected_ms = DateTime.to_unix(dt, :millisecond)

    raw = Ash.UUIDv7.bingenerate(dt)
    assert Ash.UUIDv7.extract_timestamp(raw) == expected_ms
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
