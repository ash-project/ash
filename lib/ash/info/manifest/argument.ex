# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Argument do
  @moduledoc """
  Represents an action argument or calculation argument in the API specification.
  """

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.Info.Manifest.Type.t(),
          allow_nil?: boolean(),
          has_default?: boolean(),
          description: String.t() | nil,
          sensitive?: boolean(),
          custom: map()
        }

  defstruct [
    :name,
    :type,
    :allow_nil?,
    :has_default?,
    :description,
    :sensitive?,
    custom: %{}
  ]
end
