# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.PriorityScore do
  @moduledoc """
  A custom type that represents a priority score from 1-100.
  Demonstrates custom type support in Ash.Info.Manifest.
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :integer

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) when is_integer(value) and value >= 1 and value <= 100 do
    {:ok, value}
  end

  def cast_input(value, _) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} when int >= 1 and int <= 100 -> {:ok, int}
      _ -> {:error, "must be an integer between 1 and 100"}
    end
  end

  def cast_input(_, _), do: {:error, "must be an integer between 1 and 100"}

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}
  def cast_stored(value, _) when is_integer(value), do: {:ok, value}
  def cast_stored(_, _), do: {:error, "stored value must be an integer"}

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_integer(value), do: {:ok, value}
  def dump_to_native(_, _), do: {:error, "dump value must be an integer"}

  @impl true
  def apply_constraints(value, _constraints), do: {:ok, value}
end
