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
end
