defmodule Ash.Type.Binary do
  @moduledoc """
  Represents a binary.

  A builtin type that can be referenced via `:binary`
  """

  use Ash.Type

  @impl true
  def storage_type(_), do: :binary

  @impl true
  def generator(_constraints) do
    StreamData.binary()
  end

  @impl true
  def cast_input(value, _) do
    Ecto.Type.cast(:binary, value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    case Ecto.Type.load(:binary, value) do
      {:ok, value} -> case Base.decode64(value) do
        {:ok, value} -> {:ok, value}
        :error -> {:ok, value}
      end
      :error -> :error
    end
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:binary, value)
  end

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(binary, _), do: {:ok, Base.encode64(binary)}
end
