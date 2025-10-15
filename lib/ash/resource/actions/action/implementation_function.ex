# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Action.ImplementationFunction do
  @moduledoc false
  use Ash.Resource.Actions.Implementation

  def run(input, [fun: {m, f, a}], context) do
    apply(m, f, [input, context | a])
  end

  def run(input, [fun: fun], context) do
    fun.(input, context)
  end
end
