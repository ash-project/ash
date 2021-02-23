defmodule Ash.Type.UtcDatetimeUsec do
  @moduledoc """
  Represents a utc datetime with microsecond precision.

  A builtin type that can be referenced via `:utc_datetime_usec`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :utc_datetime_usec

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:utc_datetime_usec, value)
  end

  @impl true
  def cast_stored(value, _) do
    Ecto.Type.load(:utc_datetime_usec, value)
  end

  @impl true
  def dump_to_native(value, _) do
    Ecto.Type.dump(:utc_datetime_usec, value)
  end
end
