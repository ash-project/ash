# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.AttachmentFile do
  @moduledoc """
  Embedded resource for array of unions testing - file attachment type.

  Embedded resource used as a union member inside an array.
  """
  use Ash.Resource,
    data_layer: :embedded

  attributes do
    attribute :attachment_type, :string do
      allow_nil? false
      default "file"
      public? true
    end

    attribute :filename, :string do
      allow_nil? false
      public? true
    end

    attribute :mime_type, :string do
      allow_nil? true
      public? true
    end

    attribute :is_public_1?, :boolean do
      default true
      public? true
    end

    attribute :size_bytes_1, :integer do
      allow_nil? true
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
