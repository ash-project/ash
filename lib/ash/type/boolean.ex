defmodule Ash.Type.Boolean do
  @moduledoc """
  Represents a boolean.

  A builtin type that can be referenced via `:boolean`
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :boolean

  @impl true
  def generator(_constraints) do
    StreamData.boolean()
  end

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:boolean, value)
  end

  @impl true
  def matches_type?(v, _) do
    is_boolean(v)
  end

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:boolean, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:boolean, value)
  end
end
