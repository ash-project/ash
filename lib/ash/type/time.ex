defmodule Ash.Type.Time do
  @moduledoc """
  Represents a time in the database

  A builtin type that can be referenced via `:time`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :time

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:time, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:time, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:time, value)
  end
end
