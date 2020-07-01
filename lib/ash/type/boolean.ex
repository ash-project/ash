defmodule Ash.Type.Boolean do
  @moduledoc """
  Represents a boolean.

  A builtin type that can be referenced via `:boolean`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :boolean

  @impl true
  def cast_input(value) do
    Ecto.Type.cast(:boolean, value)
  end

  @impl true
  def cast_stored(value) do
    Ecto.Type.load(:boolean, value)
  end

  @impl true
  def dump_to_native(value) do
    Ecto.Type.dump(:boolean, value)
  end
end
