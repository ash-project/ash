# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.ContentItem do
  @moduledoc """
  Union type for polymorphic content items.
  """

  use Ash.Type.NewType,
    subtype_of: :union,
    constraints: [
      types: [
        article: [
          type: :struct,
          constraints: [instance_of: Ash.Test.Manifest.Article]
        ]
      ]
    ]
end
