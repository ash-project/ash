# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ActorAbsent do
  @moduledoc "This check is true when the actor is `nil`, and false when the actor is specified."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_) do
    "actor is not present"
  end

  @impl true
  def match?(nil, _, _opts), do: true
  def match?(_, _, _opts), do: false
end
