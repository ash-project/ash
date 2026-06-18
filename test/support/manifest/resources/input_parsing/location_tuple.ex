# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.LocationTuple do
  @moduledoc """
  NewType tuple with constrained fields for the manifest type resolver.

  Exercises a `:tuple` NewType with element names that include trailing
  `?` and trailing `_N` to ensure the manifest resolves tuple shapes
  correctly.
  """
  use Ash.Type.NewType,
    subtype_of: :tuple,
    constraints: [
      fields: [
        lat_1: [type: :float, allow_nil?: false],
        lng_1: [type: :float, allow_nil?: false],
        is_verified?: [type: :boolean, allow_nil?: true]
      ]
    ]
end
