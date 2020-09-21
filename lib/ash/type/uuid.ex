defmodule Ash.Type.UUID do
  @moduledoc """
  Represents a UUID.

  A builtin type that can be referenced via `:uuid`
  """

  use Ash.Type

  @impl true
  def storage_type, do: :binary_id

  @impl true
  def cast_input(value) do
    Ecto.Type.cast(:binary_id, value)
  end

  @impl true
  def cast_stored(value) do
    Ecto.Type.load(:binary_id, value)
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:binary_id, value)
  end
end
