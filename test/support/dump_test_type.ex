# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.DumpTestType do
  @moduledoc """
  Custom type where dump_to_native and dump_to_embedded produce distinguishable results.

  Used in type tests to verify that composite types call the correct dump function
  on their field types.
  """
  use Ash.Type

  @impl true
  def storage_type(_), do: :string

  @impl true
  def cast_input(value, _), do: {:ok, value}

  @impl true
  def cast_stored("native:" <> value, _), do: {:ok, value}
  def cast_stored("embedded:" <> value, _), do: {:ok, value}
  def cast_stored(value, _), do: {:ok, value}

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}
  def dump_to_native(value, _), do: {:ok, "native:#{value}"}

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}
  def dump_to_embedded(value, _), do: {:ok, "embedded:#{value}"}

  @impl true
  def matches_type?(_, _), do: true
end
