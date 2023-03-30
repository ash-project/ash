defmodule Ash.Type.Struct do
  @moduledoc """
  Represents a struct.

  This cannot be loaded from a database, it can only be used to cast input.

  Use the `instance_of` constraint to specify that it must be an instance of a specific struct.
  """
  use Ash.Type

  @constraints [
    instance_of: [
      type: :atom,
      doc: "The module the struct should be an instance of"
    ]
  ]

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type, do: :map

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(%struct{} = value, constraints) do
    case constraints[:instance_of] do
      nil ->
        {:ok, value}

      ^struct ->
        {:ok, value}

      _ ->
        :error
    end
  end

  def cast_input(_, _), do: :error

  @impl true
  def cast_stored(_, _), do: :error

  @impl true
  def dump_to_native(_, _), do: :error
end
