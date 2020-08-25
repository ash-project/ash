defmodule Ash.Type.Atom do
  @moduledoc """
  Stores an atom as a string in the database

  A builtin type that can be referenced via `:atom`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def cast_input(value) when is_atom(value) do
    {:ok, value}
  end

  def cast_input(value) when is_binary(value) do
    {:ok, String.to_existing_atom(value)}
  rescue
    ArgumentError ->
      :error
  end

  @impl true
  def cast_stored(value) when is_atom(value) do
    {:ok, value}
  end

  def cast_stored(value) when is_binary(value) do
    {:ok, String.to_existing_atom(value)}
  rescue
    ArgumentError ->
      :error
  end

  @impl true
  def dump_to_native(value) when is_atom(value) do
    {:ok, to_string(value)}
  end

  def dump_to_native(_), do: :error
end
