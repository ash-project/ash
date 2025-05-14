defmodule Ash.Type.Duration do
  @moduledoc """
  Represents a Duration

  A builtin type that can be referenced via `:duration`
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :duration

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.constant(Duration.new!(minute: 30))
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) do
    Ecto.Type.cast(:duration, value)
  end

  @impl true
  def matches_type?(%Duration{}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_binary(value) do
    cast_input(value, constraints)
  end

  def cast_stored(value, _) do
    Ecto.Type.load(:duration, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:duration, value)
  end
end
