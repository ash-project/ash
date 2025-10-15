# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Preparation.Function do
  @moduledoc false

  use Ash.Resource.Preparation

  def supports(_opts), do: [Ash.Query, Ash.ActionInput]

  def prepare(query_or_input, [fun: {m, f, a}], context) do
    apply(m, f, [query_or_input, context | a])
  end

  def prepare(query_or_input, [fun: fun], context) do
    fun.(query_or_input, context)
  end
end
