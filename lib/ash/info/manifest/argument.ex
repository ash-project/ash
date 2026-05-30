# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Argument do
  @moduledoc """
  Represents an action argument or calculation argument in the API specification.

  `allow_nil?` and `has_default?` describe the shape of the value once supplied
  (whether `nil` is a permitted value, and whether a default exists). `required?`
  is orthogonal and describes input presence: whether a caller must supply this
  argument at all. A field can be required-to-provide but nullable, or optional-
  to-provide but non-null when provided — so consumers generating input types
  should look at `required?` to decide optionality.
  """

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.Info.Manifest.Type.t(),
          allow_nil?: boolean(),
          has_default?: boolean(),
          required?: boolean(),
          description: String.t() | nil,
          sensitive?: boolean(),
          custom: map()
        }

  defstruct [
    :name,
    :type,
    :allow_nil?,
    :has_default?,
    :required?,
    :description,
    :sensitive?,
    custom: %{}
  ]
end
