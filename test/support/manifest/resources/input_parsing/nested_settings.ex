# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.NestedSettings do
  @moduledoc """
  NewType for testing nested typed maps (map containing another typed map).

  Tests deep nesting: outer_map → inner_map, each constrained.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        display_name: [type: :string, allow_nil?: false],
        is_enabled_1?: [type: :boolean, allow_nil?: true],
        inner_config: [
          type: :map,
          allow_nil?: true,
          constraints: [
            fields: [
              max_retries_1: [type: :integer, allow_nil?: false],
              is_cached?: [type: :boolean, allow_nil?: true],
              timeout_ms: [type: :integer, allow_nil?: true]
            ]
          ]
        ]
      ]
    ]
end
