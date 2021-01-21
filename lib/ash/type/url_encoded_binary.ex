defmodule Ash.Type.UrlEncodedBinary do
  @moduledoc """
  Represents a binary that attempts to decode input strings as a url encoded base64 string.

  A builtin type that can be referenced via `:url_encoded_binary`
  """

  use Ash.Type

  @impl true
  def storage_type, do: :binary

  @impl true
  def cast_input(value) when is_binary(value) do
    case Base.url_decode64(value, padding: false) do
      {:ok, decoded} ->
        Ecto.Type.cast(:binary, decoded)

      :error ->
        Ecto.Type.cast(:binary, value)
    end
  end

  def cast_input(value) do
    Ecto.Type.cast(:binary, value)
  end

  @impl true
  def cast_stored(value) do
    Ecto.Type.load(:binary, value)
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:binary, value)
  end
end
