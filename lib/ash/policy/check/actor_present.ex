# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.ActorPresent do
  @moduledoc "This check is true when there is an actor specified, and false when the actor is `nil`."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(_) do
    "actor is present"
  end

  @impl true
  def match?(nil, _, _opts), do: false
  def match?(_, _, _opts), do: true
end
