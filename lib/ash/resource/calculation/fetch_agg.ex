# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Calculation.FetchAgg do
  @moduledoc false
  use Ash.Resource.Calculation

  def calculate(records, opts, _context) do
    if load = opts[:load] do
      Enum.map(records, fn record -> Map.get(record, load) end)
    else
      name = opts[:name]

      Enum.map(records, fn record ->
        record.aggregates[name]
      end)
    end
  end
end
