# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.DataContentMap do
  @moduledoc """
  NewType for the 'data' union member with constrained fields.

  Maps is_cached? to isCached to satisfy verifier requirements.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        item_count: [type: :integer, allow_nil?: false],
        is_cached?: [type: :boolean, allow_nil?: true]
      ]
    ]
end
