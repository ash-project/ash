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
    * `{:concrete, type_module}` — RHS is a specific Ash type module. Short-name
      aliases are resolved through `Ash.Type.get_type/1`, so this is always a
      module (e.g. `Ash.Type.String`, `Ash.Type.UUID`, never `:string`).
    * `{:array, rhs}` — RHS is an array whose element RHS is the nested value.

  ## Examples

      iex> # `:==` on a string field
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :==, rhs: :same}

      iex> # `:in` on any field — RHS is an array of the field type
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :in, rhs: {:array, :same}}

      iex> # `:is_nil` — RHS is always a boolean
      iex> %Ash.Info.Manifest.ApplicableOperator{name: :is_nil, rhs: {:concrete, Ash.Type.Boolean}}
  """

  @type rhs ::
          :same
          | :any
          | {:concrete, module()}
          | {:array, rhs()}

  @type t :: %__MODULE__{
          name: atom(),
          rhs: rhs(),
          custom: map()
        }

  defstruct [:name, :rhs, custom: %{}]
end
