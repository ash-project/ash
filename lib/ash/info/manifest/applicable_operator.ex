# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.ApplicableOperator do
  @moduledoc """
  A predicate operator resolved as applicable to a specific field, paired
  with the resolved right-hand-side type.

  `rhs` is one of:

    * `:same` — RHS is the same type as the field (e.g. `field == field_value`).
    * `:any` — RHS is untyped; the renderer chooses (typically maps to the
      renderer's `any`/`unknown`).
    * `{:concrete, type_ref}` — RHS is a specific Ash type. `type_ref` is
      either a builtin atom (e.g. `:string`, `:boolean`) or a module
      (e.g. `Ash.Type.UUID`).
    * `{:array, rhs}` — RHS is an array whose element RHS is the nested value.

  ## Examples

      iex> # `:==` on a string field
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :==, rhs: :same}

      iex> # `:in` on any field — RHS is an array of the field type
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :in, rhs: {:array, :same}}

      iex> # `:is_nil` — RHS is always a boolean
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :is_nil, rhs: {:concrete, :boolean}}
  """

  @type rhs ::
          :same
          | :any
          | {:concrete, atom() | module()}
          | {:array, rhs()}

  @type t :: %__MODULE__{
          name: atom(),
          rhs: rhs()
        }

  defstruct [:name, :rhs]
end
