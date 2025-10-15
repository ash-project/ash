# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ActorAttributeEquals do
  @moduledoc "This check is true when the value of the specified attribute of the actor equals the specified value."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "actor.#{opts[:attribute]} == #{inspect(opts[:value])}"
  end

  @impl true
  def match?(nil, _, _), do: false

  def match?(actor, _context, opts) do
    with {:ok, actor_value} <- Map.fetch(actor, opts[:attribute]),
         {:ok, desired_value} <- Keyword.fetch(opts, :value) do
      Comp.equal?(actor_value, desired_value)
    else
      _ ->
        false
    end
  end

  @impl true
  def conflicts?({__MODULE__, _}, {Ash.Policy.Check.ActorAbsent, _}, _context), do: true
  def conflicts?({Ash.Policy.Check.ActorAbsent, _}, {__MODULE__, _}, _context), do: true
  def conflicts?(_, _, _), do: false

  @impl true
  def implies?({__MODULE__, _}, {Ash.Policy.Check.ActorPresent, _}, _context), do: true
  def implies?({Ash.Policy.Check.ActorPresent, _}, {__MODULE__, _}, _context), do: false
  def implies?(_, _, _), do: false
end
