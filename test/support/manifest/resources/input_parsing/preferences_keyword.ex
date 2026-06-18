# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.PreferencesKeyword do
  @moduledoc """
  NewType keyword with constrained fields for the manifest type resolver.

  Exercises a `:keyword` NewType with field names that include trailing
  `?` and trailing `_N` to ensure the manifest resolves keyword shapes
  correctly.
  """
  use Ash.Type.NewType,
    subtype_of: :keyword,
    constraints: [
      fields: [
        theme_1: [type: :string, allow_nil?: false],
        font_size: [type: :integer, allow_nil?: true],
        is_dark_mode?: [type: :boolean, allow_nil?: true]
      ]
    ]
end
