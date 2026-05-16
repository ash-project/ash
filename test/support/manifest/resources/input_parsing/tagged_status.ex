# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.TaggedStatus do
  @moduledoc """
  Embedded resource for union member with map_with_tag storage.

  Used to test union input with :map_with_tag storage mode.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :status_type, :string do
      allow_nil? false
      public? true
    end

    attribute :message, :string do
      allow_nil? true
      public? true
    end

    attribute :is_final?, :boolean do
      default false
      public? true
    end

    attribute :priority_1, :integer do
      default 0
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
