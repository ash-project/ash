defmodule Ash.Type.Map do
  @moduledoc """
  Represents a map stored in the database.

  In postgres, for example, this represents binary encoded json

  A builtin type that can be referenced via `:map`
  """
  use Ash.Type

  @impl true
  def storage_type, do: :map

  @impl true
  def cast_input(value) when is_map(value), do: {:ok, value}
  def cast_input(_), do: :error

  @impl true
  def cast_stored(value) when is_map(value), do: {:ok, value}
  def cast_stored(_), do: :error

  @impl true
  def dump_to_native(value) when is_map(value), do: {:ok, value}
  def dump_to_native(_), do: :error
end
