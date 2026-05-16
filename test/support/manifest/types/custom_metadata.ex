# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.CustomMetadata do
  @moduledoc """
  A custom map type with constrained fields for testing.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        field_1: [type: :string, allow_nil?: false],
        is_active?: [type: :boolean, allow_nil?: false],
        line_2: [type: :string, allow_nil?: true]
      ]
    ]
end
