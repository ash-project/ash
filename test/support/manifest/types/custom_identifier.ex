# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.CustomIdentifier do
  @moduledoc """
  A custom Ash type with no special handling.

  This simulates a type from a third-party dependency where users cannot
  add the callback themselves. The type should be mapped via config.
  """
  use Ash.Type

  @impl true
  def storage_type(_constraints), do: :string

  @impl true
  def cast_input(nil, _constraints), do: {:ok, nil}
  def cast_input(value, _constraints) when is_binary(value), do: {:ok, value}
  def cast_input(_value, _constraints), do: :error

  @impl true
  def cast_stored(nil, _constraints), do: {:ok, nil}
  def cast_stored(value, _constraints) when is_binary(value), do: {:ok, value}
  def cast_stored(_value, _constraints), do: :error

  @impl true
  def dump_to_native(nil, _constraints), do: {:ok, nil}
  def dump_to_native(value, _constraints) when is_binary(value), do: {:ok, value}
  def dump_to_native(_value, _constraints), do: :error
end
