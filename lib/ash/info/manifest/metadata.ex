# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Metadata do
  @moduledoc """
  Represents action metadata in the API specification.
  """

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.Info.Manifest.Type.t(),
          allow_nil?: boolean(),
          description: String.t() | nil,
          custom: map()
        }

  defstruct [
    :name,
    :type,
    :allow_nil?,
    :description,
    custom: %{}
  ]
end
