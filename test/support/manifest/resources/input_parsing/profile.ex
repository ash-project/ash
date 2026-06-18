# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.Profile do
  @moduledoc """
  Embedded resource for the manifest type resolver with nested resources.

  Contains plain attributes alongside attributes with unusual
  identifiers (trailing `?`).
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :display_name, :string do
      allow_nil? false
      public? true
    end

    attribute :bio_text_1, :string do
      allow_nil? true
      public? true
    end

    attribute :is_public?, :boolean do
      default true
      public? true
    end

    attribute :follower_count, :integer do
      default 0
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
