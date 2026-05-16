# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.TextContent do
  @moduledoc """
  Embedded resource used as a union member.

  Used as a tagged union member to verify that union type inputs
  correctly resolve embedded resource fields.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :content_type, :string do
      allow_nil? false
      public? true
    end

    attribute :body, :string do
      allow_nil? false
      public? true
    end

    attribute :word_count_1, :integer do
      default 0
      public? true
    end

    attribute :is_formatted?, :boolean do
      default false
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
