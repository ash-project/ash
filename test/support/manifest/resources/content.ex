# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Content do
  @moduledoc """
  Test resource representing content items (articles, videos, etc) in a CMS.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets,
    primary_read_warning?: false

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id

    attribute :type, :atom do
      constraints one_of: [:article]
      allow_nil? false
      default :article
      public? true
    end

    attribute :title, :string do
      allow_nil? false
      public? true
    end

    attribute :thumbnail_url, :string do
      allow_nil? false
      public? true
    end

    attribute :thumbnail_alt, :string do
      allow_nil? false
      public? true
    end

    attribute :published_at, :utc_datetime do
      allow_nil? true
      public? true
    end

    attribute :category, :atom do
      constraints one_of: [:fitness, :nutrition, :mindset, :progress]
      allow_nil? false
      default :nutrition
      public? true
    end

    create_timestamp :created_at do
      public? true
    end

    update_timestamp :updated_at do
      public? true
    end
  end

  calculations do
    calculate :item, Ash.Test.Manifest.ContentItem, Ash.Test.Manifest.ItemCalculation do
      public? true
    end
  end

  relationships do
    has_one :article, Ash.Test.Manifest.Article do
      public? true
    end

    belongs_to :author, Ash.Test.Manifest.User do
      public? true
    end
  end

  actions do
    defaults [:destroy]

    read :read do
      primary? true

      pagination required?: false,
                 offset?: true,
                 keyset?: true

      argument :include_unpublished, :boolean do
        allow_nil? true
        default false
      end
    end

    read :get_by_id do
      get_by :id

      argument :include_unpublished, :boolean do
        allow_nil? true
        default false
      end
    end

    create :create do
      primary? true

      accept [
        :type,
        :title,
        :thumbnail_url,
        :thumbnail_alt,
        :published_at,
        :category,
        :author_id
      ]

      argument :item, :map do
        allow_nil? false

        constraints fields: [
                      hero_image_url: [type: :string],
                      hero_image_alt: [type: :string],
                      summary: [type: :string],
                      body: [type: :string, allow_nil?: false]
                    ]
      end

      argument :user_id, :uuid do
        allow_nil? false
      end

      change manage_relationship(:item, :article, type: :create)
      change manage_relationship(:user_id, :author, type: :append)
    end

    update :update do
      primary? true
      require_atomic? false

      accept [
        :type,
        :title,
        :thumbnail_url,
        :thumbnail_alt,
        :published_at,
        :category
      ]

      argument :item, :map do
        allow_nil? true
      end

      change manage_relationship(:item, :article, type: :direct_control)
    end
  end
end
