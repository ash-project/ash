defmodule Ash.Type.NaiveDatetime do
  @moduledoc """
  Represents a Naive datetime

  A builtin type that can be referenced via `:naive_datetime`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :naive_datetime

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) do
    Ecto.Type.cast(:naive_datetime, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Ecto.Type.load(:naive_datetime, value)
  end

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:naive_datetime, value)
  end
end
