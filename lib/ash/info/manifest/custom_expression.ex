# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.CustomExpression do
  @moduledoc """
  Represents an app-registered custom expression in the filter capabilities catalog.

  Built from `Application.get_env(:ash, :custom_expressions, [])` at manifest
  generation time. Does not carry a `:returns` field — `Ash.CustomExpression`
  has no `returns/0` callback.
  """

  alias Ash.Info.Manifest.ArgumentSignature

  @type t :: %__MODULE__{
          name: atom(),
          module: module(),
          predicate?: boolean(),
          signatures: [ArgumentSignature.t()],
          description: String.t() | nil,
          custom: map()
        }

  defstruct [:name, :module, :predicate?, :signatures, :description, custom: %{}]
end
