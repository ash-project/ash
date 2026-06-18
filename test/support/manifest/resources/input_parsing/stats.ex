# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.Stats do
  @moduledoc """
  NewType with constrained fields for the manifest type resolver.

  Exercises field names that include unusual identifiers (trailing `?`,
  trailing `_N`) to ensure they round-trip through the manifest as-is.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        total_count_1: [type: :integer, allow_nil?: false],
        is_complete?: [type: :boolean, allow_nil?: true],
        last_updated_at: [type: :utc_datetime, allow_nil?: true]
      ]
    ]
end
