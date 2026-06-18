# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.ColorPalette do
  @moduledoc """
  A custom type that represents a color palette stored as a map.
  Demonstrates how custom types can provide precise structural definitions
  for complex data structures.
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :map

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  # Accept atom keys
  def cast_input(%{primary: primary, secondary: secondary, accent: accent} = value, _)
      when is_binary(primary) and is_binary(secondary) and is_binary(accent) do
    {:ok, value}
  end

  # Accept string keys (from client input without field constraints)
  def cast_input(
        %{"primary" => primary, "secondary" => secondary, "accent" => accent},
        _
      )
      when is_binary(primary) and is_binary(secondary) and is_binary(accent) do
    {:ok, %{primary: primary, secondary: secondary, accent: accent}}
  end

  def cast_input(_, _), do: {:error, "must be a map with primary, secondary, and accent colors"}

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}
  def cast_stored(value, _) when is_map(value), do: {:ok, value}
  def cast_stored(_, _), do: {:error, "stored value must be a map"}

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _) when is_map(value), do: {:ok, value}
  def dump_to_native(_, _), do: {:error, "dump value must be a map"}

  @impl true
  def apply_constraints(value, _constraints), do: {:ok, value}
end
