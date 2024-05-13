defmodule Ash.Type.Term do
  @moduledoc """
  Represents a raw elixir term in the database

  A builtin type that can be referenced via `:binary`
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :binary

  @impl true
  def cast_input(value, _), do: {:ok, value}

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}
  # sobelow_skip ["Misc.BinToTerm"]
  def cast_stored(value, _), do: {:ok, Ash.Helpers.non_executable_binary_to_term(value, [:safe])}

  @impl true

  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _), do: {:ok, :erlang.term_to_binary(value)}
end
