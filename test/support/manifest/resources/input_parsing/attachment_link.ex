# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.AttachmentLink do
  @moduledoc """
  Embedded resource for array of unions testing - link attachment type.

  Embedded resource used as a union member inside an array.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :attachment_type, :string do
      allow_nil? false
      default "link"
      public? true
    end

    attribute :url, :string do
      allow_nil? false
      public? true
    end

    attribute :title, :string do
      allow_nil? true
      public? true
    end

    attribute :is_external_1?, :boolean do
      default true
      public? true
    end

    attribute :click_count_1, :integer do
      default 0
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
