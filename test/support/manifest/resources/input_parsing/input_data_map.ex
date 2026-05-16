# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.InputDataMap do
  @moduledoc """
  NewType for the process_data action's input_data argument.

  Maps is_valid? to isValid to satisfy verifier requirements.
  """
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        item_name: [type: :string, allow_nil?: false],
        is_valid?: [type: :boolean, allow_nil?: true]
      ]
    ]
end
