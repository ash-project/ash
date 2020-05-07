defmodule Ash.Type.Atom do
  use Ash.Type

  @impl true
  def storage_type(), do: :string

  @impl true
  def describe(), do: "A standard elixir atom, stored as a string."

  @impl true
  def cast_input(value) when is_atom(value), do: {:ok, value}
  def cast_input(_), do: :error

  @impl true
  def cast_stored(value), do: {:ok, String.to_existing_atom(value)}

  @impl true
  def dump_to_native(value) when is_atom(value), do: {:ok, Atom.to_string(value)}
  def dump_to_native(_), do: :error
end
