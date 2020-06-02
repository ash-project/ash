defmodule Ash.Type.Term do
  use Ash.Type

  @impl true
  def storage_type(), do: :string

  @impl true
  def describe(), do: "An Elixir value stored as a string"

  @impl true
  def cast_input(value), do: {:ok, value}

  @impl true
  # sobelow_skip ["Misc.BinToTerm"]
  def cast_stored(value), do: {:ok, :erlang.binary_to_term(value, [:safe])}

  @impl true
  def dump_to_native(value), do: {:ok, :erlang.term_to_binary(value)}

  @impl true
  def supported_filter_types(_data_layer), do: []

  @impl true
  def sortable?(_data_layer), do: false
end
