defmodule Ash.Type.UrlEncodedBinary do
  @moduledoc """
  Represents a binary that attempts to decode input strings as a url encoded base64 string.

  A builtin type that can be referenced via `:url_encoded_binary`
  """

  use Ash.Type

  @impl true
  def storage_type(_), do: :binary

  @impl true
  def generator(_constraints) do
    base64 =
      [?A..?Z, ?a..?z, ?0..?9, ?+, ?/]
      |> StreamData.string()
      |> StreamData.map(&Base.encode64/1)

    StreamData.one_of([StreamData.binary(), base64])
  end

  @impl true
  def matches_type?(value, _), do: is_binary(value)

  @impl true
  def cast_input(value, _) when is_binary(value) do
    case Base.url_decode64(value, padding: false) do
      {:ok, decoded} ->
        Ecto.Type.cast(:binary, decoded)

      :error ->
        Ecto.Type.cast(:binary, value)
    end
  end

  def cast_input(value, _) do
    Ecto.Type.cast(:binary, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:binary, value)
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:binary, value)
  end
end
