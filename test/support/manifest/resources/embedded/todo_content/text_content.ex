# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoContent.TextContent do
  @moduledoc """
  Test embedded resource for text content (union type member).
  """
  use Ash.Resource,
    data_layer: :embedded,
    domain: nil

  attributes do
    uuid_primary_key :id

    attribute :text, :string, public?: true, allow_nil?: false

    attribute :formatting, :atom,
      public?: true,
      constraints: [one_of: [:plain, :markdown, :html]],
      default: :plain

    attribute :word_count, :integer, public?: true, default: 0
    attribute :content_type, :string, public?: true, default: "text"
  end

  calculations do
    calculate :display_text, :string, expr(text) do
      public? true
    end

    calculate :is_formatted, :boolean, expr(formatting != :plain) do
      public? true
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true
      accept [:text, :formatting, :word_count]
    end
  end
end
