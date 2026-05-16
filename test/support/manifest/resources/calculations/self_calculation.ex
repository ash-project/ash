# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.SelfCalculation do
  @moduledoc """
  Self-reference calculation for testing.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    []
  end

  @impl true
  def calculate(records, _opts, _context) do
    # Just return the records unchanged for testing purposes
    # In a real implementation, you might modify based on the prefix argument
    records
  end
end
