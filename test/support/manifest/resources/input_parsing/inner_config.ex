# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.InnerConfig do
  @moduledoc """
  NewType for inner config with constrained fields.

  Used to exercise nested typed maps where both outer and inner are constrained.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        max_retries_1: [type: :integer, allow_nil?: false],
        is_cached?: [type: :boolean, allow_nil?: true],
        timeout_ms: [type: :integer, allow_nil?: true]
      ]
    ]
end
