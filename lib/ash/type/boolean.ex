defmodule Ash.Type.Boolean do
  @moduledoc "Stores a boolean in the database"
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
