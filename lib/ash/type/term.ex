defmodule Ash.Type.Term do
  @moduledoc """
  Represents a raw elixir term in the database

  A builtin type that can be referenced via `:string`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :string

  @impl true
  def cast_input(value), do: {:ok, value}

  @impl true
  # sobelow_skip ["Misc.BinToTerm"]
  def cast_stored(value), do: {:ok, :erlang.binary_to_term(value, [:safe])}

  @impl true
  def dump_to_native(value), do: {:ok, :erlang.term_to_binary(value)}
end
