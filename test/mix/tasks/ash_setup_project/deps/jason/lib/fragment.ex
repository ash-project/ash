defmodule Jason.Fragment do
  @moduledoc ~S"""
  Provides a way to inject an already-encoded JSON structure into a
  to-be-encoded structure in optimized fashion.

  This avoids a decoding/encoding round-trip for the subpart.

  This feature can be used for caching parts of the JSON, or delegating
  the generation of the JSON to a third-party system (e.g. Postgres).
  """

  defstruct [:encode]

  def new(iodata) when is_list(iodata) or is_binary(iodata) do
    %__MODULE__{encode: fn _ -> iodata end}
  end

  def new(encode) when is_function(encode, 1) do
    %__MODULE__{encode: encode}
  end
end
