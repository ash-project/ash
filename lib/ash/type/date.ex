defmodule Ash.Type.Date do
  @moduledoc """
  Represents a date in the database

  A builtin type that can be referenced via `:date`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :date

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:date, value)
  end

  @impl true
  def cast_stored(value, _) do
    Ecto.Type.load(:date, value)
  end

  @impl true
  def dump_to_native(value, _) do
    Ecto.Type.dump(:date, value)
  end
end
