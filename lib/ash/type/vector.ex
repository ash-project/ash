# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Vector do
  @constraints [
    dimensions: [
      type: :integer,
      doc: "The dimensions of the vector"
    ]
  ]
  @moduledoc """
  Represents a vector.

  A builtin type that can be referenced via `:vector`.

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """

  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_constraints) do
    :vector
  end

  @impl true
  def generator(_constraints) do
    StreamData.list_of(StreamData.float())
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, constraints) do
    with {:ok, vector} <- Ash.Vector.new(value) do
      dims = constraints[:dimensions]

      if dims && vector.dimensions != dims do
        {:error, "must have #{dims} dimensions"}
      else
        {:ok, vector}
      end
    end
  end

  @impl true
  def matches_type?(%Ash.Vector{}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(%Ash.Vector{} = vector, _) do
    {:ok, vector}
  end

  def cast_stored(value, _) when is_list(value) do
    case Ash.Vector.new(value) do
      {:ok, vector} -> {:ok, vector}
      {:error, _} -> :error
    end
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(%Ash.Vector{} = value, _) do
    {:ok, value}
  end

  def dump_to_native(value, constraints) when is_list(value) do
    with {:ok, value} <- cast_input(value, constraints) do
      dump_to_native(value, constraints)
    end
  end
end
