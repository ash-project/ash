# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.ProcessProfileResult do
  @moduledoc """
  NewType for process_profile action return type with constrained fields.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        profile_name: [type: :string, allow_nil?: false],
        is_processed?: [type: :boolean, allow_nil?: false]
      ]
    ]
end
