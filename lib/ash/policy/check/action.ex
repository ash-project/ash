# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.Action do
  @moduledoc "This check is true when the action name matches the provided action name."
  use Ash.Policy.SimpleCheck

  import Crux.Expression, only: [b: 1]

  @impl true
  def describe(options) do
    {operator, action} =
      case options[:action] do
        [action] -> {"==", action}
        actions -> {"in", actions}
      end

    "action #{operator} #{inspect(action)}"
  end

  @impl true
  def match?(_actor, %{action: %{name: name}}, options) do
    name in options[:action]
  end

  @impl true
  def simplify({__MODULE__, options}, _context) do
    case List.wrap(options[:action]) do
      [] ->
        false

      [_ | _] = multiple_actions ->
        multiple_actions
        |> Enum.map(&{__MODULE__, Keyword.put(options, :action, [&1])})
        |> Enum.reduce(&b(&2 or &1))
    end
  end

  @impl true
  def conflicts?({__MODULE__, opts1}, {__MODULE__, opts2}, _context) do
    actions1 = opts1[:action] |> List.wrap() |> MapSet.new()
    actions2 = opts2[:action] |> List.wrap() |> MapSet.new()

    not Enum.empty?(actions1) and not Enum.empty?(actions2) and
      MapSet.disjoint?(actions1, actions2)
  end

  def conflicts?({__MODULE__, action_opts}, {Ash.Policy.Check.ActionType, type_opts}, context) do
    actions = action_opts[:action] |> List.wrap()
    types = type_opts[:type] |> List.wrap()

    action_types =
      actions
      |> Enum.map(&Ash.Resource.Info.action(context.resource, &1))
      |> Enum.reject(&is_nil/1)
      |> Enum.map(& &1.type)
      |> MapSet.new()

    type_set = MapSet.new(types)

    not Enum.empty?(action_types) and not Enum.empty?(type_set) and
      MapSet.disjoint?(action_types, type_set)
  end

  def conflicts?({Ash.Policy.Check.ActionType, type_opts}, {__MODULE__, action_opts}, context) do
    conflicts?({__MODULE__, action_opts}, {Ash.Policy.Check.ActionType, type_opts}, context)
  end

  def conflicts?(_, _, _), do: false

  @impl true
  def implies?({__MODULE__, action_opts}, {Ash.Policy.Check.ActionType, type_opts}, context) do
    actions = action_opts[:action] |> List.wrap()
    types = type_opts[:type] |> List.wrap()

    action_types =
      actions
      |> Enum.map(&Ash.Resource.Info.action(context.resource, &1))
      |> Enum.reject(&is_nil/1)
      |> Enum.map(& &1.type)
      |> MapSet.new()

    type_set = MapSet.new(types)

    not Enum.empty?(action_types) and MapSet.subset?(action_types, type_set)
  end

  def implies?({Ash.Policy.Check.ActionType, type_opts}, {__MODULE__, action_opts}, context) do
    actions = action_opts[:action] |> List.wrap()
    types = type_opts[:type] |> List.wrap()

    if length(actions) == 1 do
      action = hd(actions)
      action_info = Ash.Resource.Info.action(context.resource, action)

      if action_info do
        action_info.type in types
      else
        false
      end
    else
      false
    end
  end

  def implies?(_, _, _), do: false
end
