# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoContent.LinkContent do
  @moduledoc """
  Test embedded resource for link content (union type member).
  """
  use Ash.Resource,
    data_layer: :embedded,
    domain: nil

  attributes do
    uuid_primary_key :id

    attribute :url, :string,
      public?: true,
      allow_nil?: false,
      constraints: [match: ~r/^https?:\/\//]

    attribute :title, :string, public?: true
    attribute :description, :string, public?: true
    attribute :preview_image_url, :string, public?: true
    attribute :is_external, :boolean, public?: true, default: true
    attribute :last_checked_at, :utc_datetime, public?: true
    attribute :content_type, :string, public?: true, default: "link"
  end

  calculations do
    calculate :display_title, :string, expr(if(is_nil(title), url, title)) do
      public? true
    end

    calculate :domain, :string, expr("example.com") do
      # In a real implementation, this would extract the domain from URL
      public? true
    end

    calculate :is_accessible, :boolean, expr(true) do
      # In a real implementation, this would check if URL is accessible
      public? true
    end
  end

  validations do
    validate present(:url), message: "URL is required"

    validate match(:url, ~r/^https?:\/\//) do
      message "URL must start with http or https"
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true

      accept [
        :url,
        :title,
        :description,
        :preview_image_url,
        :is_external,
        :last_checked_at,
        :content_type
      ]
    end
  end
end
