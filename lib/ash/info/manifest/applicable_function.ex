# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.ApplicableFunction do
  @moduledoc """
  A predicate function resolved as applicable to a specific field, paired
  with the resolved right-hand-side type.

  See `Ash.Info.Manifest.ApplicableOperator` for the shape of `rhs`.
  """

  @type t :: %__MODULE__{
          name: atom(),
          rhs: Ash.Info.Manifest.ApplicableOperator.rhs(),
          custom: map()
        }

  defstruct [:name, :rhs, custom: %{}]
end
