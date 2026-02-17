# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Crux.Expression.RewriteRule.IdempotentLaw do
  @moduledoc """
  Rewrite rule that applies idempotent laws to simplify expressions.

  See: https://en.wikipedia.org/wiki/Idempotence

  Applies the transformations:
  - `A AND A = A`
  - `A OR A = A`

  The idempotent laws state that applying the same operation twice
  has the same effect as applying it once.
  """

  use Crux.Expression.RewriteRule

  @impl Crux.Expression.RewriteRule
  def walk({op, left, right}) do
    list =
      left
      |> gather(op)
      |> Enum.concat(gather(right, op))

    uniq =
      Enum.uniq(list)

    case uniq do
      [single] ->
        single

      multiple ->
        if Enum.count(list) == Enum.count(uniq) do
          {op, left, right}
        else
          Enum.reduce(multiple, &{op, &2, &1})
        end
    end
  end

  def walk(other), do: other

  defp gather({op, left, right}, op) do
    gather(left, op) ++ gather(right, op)
  end

  defp gather(other, _), do: [other]
end
