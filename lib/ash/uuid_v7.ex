defmodule Ash.UUIDv7 do
  @moduledoc """
  Helpers for working with UUIDs version 7.

  [RFC 9562](https://www.rfc-editor.org/rfc/rfc9562#name-uuid-version-7)

  Used for generating UUIDs version 7 with increased clock precision for better monotonicity,
  as described by method 3 of the [Section 6.2](https://www.rfc-editor.org/rfc/rfc9562#name-monotonicity-and-counters

  Inspired by the work of [Ryan Winchester](https://github.com/ryanwinchester/) on [uuidv7](https://github.com/ryanwinchester/uuidv7)

  ## Examples

      iex> UUIDv7.generate()
      "018e90d8-06e8-7f9f-bfd7-6730ba98a51b"

      iex> UUIDv7.bingenerate()
      <<1, 142, 144, 216, 6, 232, 127, 159, 191, 215, 103, 48, 186, 152, 165, 27>>

  """

  @typedoc """
  A hex-encoded UUID string.
  """
  @type t :: <<_::288>>

  @typedoc """
  A raw binary representation of a UUID.
  """
  @type raw :: <<_::128>>

  @version 7
  @variant 2

  @doc """
  Generates a version 7 UUID using submilliseconds for increased clock precision.

  ## Example

      iex> UUIDv7.generate()
      "018e90d8-06e8-7f9f-bfd7-6730ba98a51b"

  """
  @spec generate() :: t
  def generate, do: bingenerate() |> encode()

  @doc """
  Generates a version 7 UUID in the binary format.

  ## Example

      iex> UUIDv7.bingenerate()
      <<1, 142, 144, 216, 6, 232, 127, 159, 191, 215, 103, 48, 186, 152, 165, 27>>

  """
  @spec bingenerate() :: raw
  def bingenerate do
    timestamp_nanoseconds = System.system_time(:nanosecond)
    timestamp_milliseconds = trunc(timestamp_nanoseconds / 1_000_000)

    nanoseconds = timestamp_nanoseconds - timestamp_milliseconds * 1_000_000
    rand_a = trunc(4096 * (nanoseconds / 1_000_000))

    <<rand_b::62, _::2>> = :crypto.strong_rand_bytes(8)

    <<
      timestamp_milliseconds::big-unsigned-48,
      @version::4,
      rand_a::12,
      @variant::2,
      rand_b::62
    >>
  end

  @doc """
  Extract the millisecond timestamp from the UUID.

  ## Example

      iex> UUIDv7.extract_timestamp("018ecb40-c457-73e6-a400-000398daddd9")
      1712807003223

  """
  @spec extract_timestamp(t | raw) :: integer
  def extract_timestamp(<<timestamp_ms::big-unsigned-48, @version::4, _::76>>), do: timestamp_ms

  def extract_timestamp(<<_::288>> = uuid) do
    uuid |> decode() |> extract_timestamp()
  end

  @doc """
  Encode a raw UUID to the string representation.

  ## Example

      iex> UUIDv7.encode(<<1, 142, 144, 216, 6, 232, 127, 159, 191, 215, 103, 48, 186, 152, 165, 27>>)
      "018e90d8-06e8-7f9f-bfd7-6730ba98a51b"

  """
  @spec encode(t | raw) :: t | :error
  def encode(
        <<_, _, _, _, _, _, _, _, ?-, _, _, _, _, ?-, _, _, _, _, ?-, _, _, _, _, ?-, _, _, _, _,
          _, _, _, _, _, _, _, _>> = hex_uuid
      ),
      do: hex_uuid

  def encode(
        <<a1::4, a2::4, a3::4, a4::4, a5::4, a6::4, a7::4, a8::4, b1::4, b2::4, b3::4, b4::4,
          c1::4, c2::4, c3::4, c4::4, d1::4, d2::4, d3::4, d4::4, e1::4, e2::4, e3::4, e4::4,
          e5::4, e6::4, e7::4, e8::4, e9::4, e10::4, e11::4, e12::4>>
      ) do
    <<e(a1), e(a2), e(a3), e(a4), e(a5), e(a6), e(a7), e(a8), ?-, e(b1), e(b2), e(b3), e(b4), ?-,
      e(c1), e(c2), e(c3), e(c4), ?-, e(d1), e(d2), e(d3), e(d4), ?-, e(e1), e(e2), e(e3), e(e4),
      e(e5), e(e6), e(e7), e(e8), e(e9), e(e10), e(e11), e(e12)>>
  end

  def encode(_), do: :error

  @compile {:inline, e: 1}

  defp e(0), do: ?0
  defp e(1), do: ?1
  defp e(2), do: ?2
  defp e(3), do: ?3
  defp e(4), do: ?4
  defp e(5), do: ?5
  defp e(6), do: ?6
  defp e(7), do: ?7
  defp e(8), do: ?8
  defp e(9), do: ?9
  defp e(10), do: ?a
  defp e(11), do: ?b
  defp e(12), do: ?c
  defp e(13), do: ?d
  defp e(14), do: ?e
  defp e(15), do: ?f

  @doc """
  Decode a string representation of a UUID to the raw binary version.

  ## Example

      iex> UUIDv7.decode("018e90d8-06e8-7f9f-bfd7-6730ba98a51b")
      <<1, 142, 144, 216, 6, 232, 127, 159, 191, 215, 103, 48, 186, 152, 165, 27>>

  """
  @spec decode(t | raw) :: raw | :error
  def decode(
        <<_::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4,
          _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4, _::4,
          _::4, _::4, _::4, _::4>> = raw_uuid
      ),
      do: raw_uuid

  def decode(
        <<a1, a2, a3, a4, a5, a6, a7, a8, ?-, b1, b2, b3, b4, ?-, c1, c2, c3, c4, ?-, d1, d2, d3,
          d4, ?-, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12>>
      ) do
    <<d(a1)::4, d(a2)::4, d(a3)::4, d(a4)::4, d(a5)::4, d(a6)::4, d(a7)::4, d(a8)::4, d(b1)::4,
      d(b2)::4, d(b3)::4, d(b4)::4, d(c1)::4, d(c2)::4, d(c3)::4, d(c4)::4, d(d1)::4, d(d2)::4,
      d(d3)::4, d(d4)::4, d(e1)::4, d(e2)::4, d(e3)::4, d(e4)::4, d(e5)::4, d(e6)::4, d(e7)::4,
      d(e8)::4, d(e9)::4, d(e10)::4, d(e11)::4, d(e12)::4>>
  catch
    :error -> :error
  end

  def decode(_), do: :error

  @compile {:inline, d: 1}

  defp d(?0), do: 0
  defp d(?1), do: 1
  defp d(?2), do: 2
  defp d(?3), do: 3
  defp d(?4), do: 4
  defp d(?5), do: 5
  defp d(?6), do: 6
  defp d(?7), do: 7
  defp d(?8), do: 8
  defp d(?9), do: 9
  defp d(?A), do: 10
  defp d(?B), do: 11
  defp d(?C), do: 12
  defp d(?D), do: 13
  defp d(?E), do: 14
  defp d(?F), do: 15
  defp d(?a), do: 10
  defp d(?b), do: 11
  defp d(?c), do: 12
  defp d(?d), do: 13
  defp d(?e), do: 14
  defp d(?f), do: 15
  defp d(_), do: throw(:error)
end
