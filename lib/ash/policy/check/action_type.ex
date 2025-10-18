# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ActionType do
  @moduledoc "This check is true when the action type matches the provided type"
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(options) do
    {operator, type} =
      case options[:type] do
        [type] -> {"==", type}
        types -> {"in", types}
      end

    "action.type #{operator} #{inspect(type)}"
  end

  @impl true
  def match?(_actor, %{action: %{type: type}}, options) do
    type in options[:type]
  end

  @impl true
  def simplify({__MODULE__, options}, context) do
    context.action.type in options[:type]
  end

  @impl true
  def conflicts?({__MODULE__, opts1}, {__MODULE__, opts2}, _context) do
    types1 = opts1[:type] |> List.wrap() |> MapSet.new()
    types2 = opts2[:type] |> List.wrap() |> MapSet.new()

    not Enum.empty?(types1) and not Enum.empty?(types2) and MapSet.disjoint?(types1, types2)
  end

  def conflicts?(_, _, _), do: false
end
