# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.SummaryCalculation do
  @moduledoc """
  Summary calculation for todos.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    []
  end

  @impl true
  def calculate(records, _opts, _context) do
    Enum.map(records, fn _record ->
      # Return a sample TodoStatistics struct for testing
      %Ash.Test.Manifest.TodoStatistics{
        view_count: 42,
        edit_count: 7,
        completion_time_seconds: 1800,
        difficulty_rating: 3.5,
        all_completed?: false,
        performance_metrics: %{
          focus_time_seconds: 900,
          interruption_count: 3,
          efficiency_score: 0.85,
          task_complexity: "medium"
        }
      }
    end)
  end
end
