# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.Options do
  @moduledoc """
  NewType for the manifest type resolver with action arguments.

  Tests constrained fields with argument types.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        cache_enabled_1?: [type: :boolean, allow_nil?: true],
        retry_limit: [type: :integer, allow_nil?: true]
      ]
    ]
end
