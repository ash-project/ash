defmodule Ash.Type.UUID do
  @moduledoc """
  Represents a UUID.

  A builtin type that can be referenced via `:uuid`
  """

  use Ash.Type

  @impl true
  def storage_type, do: :uuid

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.integer()
    |> StreamData.bind(fn _i -> StreamData.constant(Ash.UUID.generate()) end)
    |> StreamData.unshrinkable()
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) when is_binary(value) do
    case String.valid?(value) do
      true -> Ecto.Type.cast(Ecto.UUID, String.trim(value))
      false -> Ecto.Type.cast(Ecto.UUID, value)
    end
  end

  def cast_input(value, _) do
    Ecto.Type.cast(Ecto.UUID, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) do
    case Ecto.Type.load(Ecto.UUID, value) do
      :error ->
        cast_input(value, constraints)

      {:ok, value} ->
        {:ok, value}
    end
  rescue
    _e in ArgumentError ->
      cast_input(value, constraints)
  end

  @impl true
  def dump_to_embedded(value, constraints) do
    cast_input(value, constraints)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(Ecto.UUID, value)
  end
end
