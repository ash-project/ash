defmodule Ash.Type.Binary do
  @moduledoc """
  Represents a binary.

  A builtin type that can be referenced via `:binary`
  """

  use Ash.Type

  @impl true
  def storage_type, do: :binary

  @impl true
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
