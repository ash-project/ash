# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.DeepNestedSettings do
  @moduledoc """
  NewType for testing deeply nested typed maps, constrained at each level.

  Tests: outer_map (constrained fields) → inner_config (NewType with constrained fields)
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        display_name: [type: :string, allow_nil?: false],
        is_enabled_1?: [type: :boolean, allow_nil?: true],
        inner_config: [
          type: Ash.Test.Manifest.InputParsing.InnerConfig,
          allow_nil?: true
        ]
      ]
    ]
end
