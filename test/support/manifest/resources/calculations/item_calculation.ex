# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.ItemCalculation do
  @moduledoc """
  Item calculation that returns the content's article as a union type.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:article]
  end

  @impl true
  def strict_loads?, do: false

  @impl true
  def calculate(records, _opts, _context) do
    Enum.map(records, fn record ->
      if record.article do
        %Ash.Union{type: :article, value: record.article}
      else
        nil
      end
    end)
  end
end
