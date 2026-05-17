# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.FakeDataLayerFunction do
  @moduledoc """
  A fake Ash query function that pretends to be supplied by a data layer.
  Used by manifest tests to verify that data-layer-sourced functions are
  collected into `filter_capabilities.functions` and tagged with their
  `data_layer_module`.

  Mirrors `Ash.Query.Function.Contains`'s shape — single-string LHS, predicate.
  """
  use Ash.Query.Function, name: :fake_ilike, predicate?: true

  def args, do: [[:string, :string]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [left, right]}) when is_binary(left) and is_binary(right) do
    {:known, String.contains?(String.downcase(left), String.downcase(right))}
  end

  def evaluate(_), do: :unknown
end
