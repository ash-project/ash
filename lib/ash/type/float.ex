defmodule Ash.Type.Float do
  @moduledoc """
  Represents a float (floating point number)

  A builtin type that be referenced via `:float`
  """

  use Ash.Type

  @impl true
  def storage_type, do: :float

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:float, value)
  end

  @impl true
  def cast_stored(value, _) do
    Ecto.Type.load(:float, value)
  end

  @impl true
  def dump_to_native(value, _) do
    Ecto.Type.dump(:float, value)
  end
end
