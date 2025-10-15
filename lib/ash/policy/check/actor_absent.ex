# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ActorAbsent do
  @moduledoc "This check is true when the actor is `nil`, and false when the actor is specified."
  use Ash.Policy.SimpleCheck

  import Crux.Expression, only: [b: 1]

  @impl true
  def describe(_) do
    "actor is not present"
  end

  @impl true
  def match?(nil, _, _opts), do: true
  def match?(_, _, _opts), do: false

  @impl true
  def simplify({__MODULE__, opts}, _context) do
    b(not {Ash.Policy.Check.ActorPresent, opts})
  end

  @impl true
  def conflicts?({__MODULE__, _}, {Ash.Policy.Check.ActorPresent, _}, _context), do: true
  def conflicts?({Ash.Policy.Check.ActorPresent, _}, {__MODULE__, _}, _context), do: true
  def conflicts?(_, _, _), do: false
end
