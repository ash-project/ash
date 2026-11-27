# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Expressions.AtZone do
  @moduledoc false
  use Ash.CustomExpression,
    name: :at_zone,
    arguments: [
      [{:datetime, precision: :microsecond}, :string]
    ]

  def expression(data_layer, [datetime, time_zone])
      when data_layer in [
             Ash.DataLayer.Ets,
             Ash.DataLayer.Simple
           ] do
    {:ok, expr(fragment(&__MODULE__.at_zone/2, ^datetime, ^time_zone))}
  end

  def expression(_data_layer, _args), do: :unknown

  def at_zone(nil, _time_zone), do: nil
  def at_zone(_datetime, nil), do: nil

  def at_zone(datetime, time_zone) do
    case DateTime.shift_zone(datetime, time_zone) do
      {:ok, shifted} -> DateTime.to_naive(shifted)
      {:error, :time_zone_not_found} -> nil
    end
  end
end
