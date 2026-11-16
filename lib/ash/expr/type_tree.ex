# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Expr.TypeTree do
  @moduledoc """
  The type of an expression, along with the type trees of the operands it was resolved
  from. Built by `Ash.Expr.determine_type_tree/1`.

  * `expr` - the expression.
  * `type` - its type as `Ash.Expr.determine_type/1` returns it: `{:ok, {type, constraints}}`,
    or `:error` if it can't be determined.
  * `operands` - for an operator or function call whose type is resolved from its operands,
    a type tree for each operand, in order (`[left, right]` for an operator, otherwise the
    call's arguments). `nil` for anything else.
  """

  defstruct [:expr, :type, :operands]

  @type t :: %__MODULE__{
          expr: term,
          type: {:ok, {Ash.Type.t(), Keyword.t()}} | :error,
          operands: [t] | nil
        }
end
