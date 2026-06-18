# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo.SimpleDateCalculation do
  @moduledoc """
  Simple date difference calculation for todos.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:due_date]
  end

  @impl true
  def calculate(records, _opts, _context) do
    today = Date.utc_today()

    Enum.map(records, fn record ->
      if is_nil(record.due_date) do
        nil
      else
        Date.diff(record.due_date, today)
      end
    end)
  end
end

defmodule Ash.Test.Manifest.IsOverdueCalculation do
  @moduledoc """
  Calculation to determine if a todo is overdue.
  """
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:due_date]
  end

  @impl true
  def calculate(records, _opts, _context) do
    today = Date.utc_today()

    Enum.map(records, fn record ->
      if is_nil(record.due_date) do
        false
      else
        Date.compare(record.due_date, today) == :lt
      end
    end)
  end
end
