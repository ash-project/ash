# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Article do
  @moduledoc """
  Test resource representing article content with details.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id

    attribute :hero_image_url, :string do
      allow_nil? false
      public? true
    end

    attribute :hero_image_alt, :string do
      allow_nil? false
      public? true
    end

    attribute :summary, :string do
      allow_nil? false
      public? true
    end

    attribute :body, :string do
      allow_nil? false
      public? true
    end

    create_timestamp :created_at do
      public? true
    end

    update_timestamp :updated_at do
      public? true
    end
  end

  relationships do
    belongs_to :content, Ash.Test.Manifest.Content do
      allow_nil? false
      attribute_writable? true
      public? true
    end
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      primary? true

      accept [
        :content_id,
        :hero_image_url,
        :hero_image_alt,
        :summary,
        :body
      ]
    end

    update :update do
      primary? true

      accept [
        :hero_image_url,
        :hero_image_alt,
        :summary,
        :body
      ]
    end

    create :create_with_optional_hero_image do
      accept [
        :content_id,
        :hero_image_url,
        :hero_image_alt,
        :summary,
        :body
      ]

      # hero_image_url has allow_nil?: false on the attribute,
      # but this action allows nil input for it
      allow_nil_input [:hero_image_url]
    end

    update :update_with_required_hero_image_alt do
      accept [
        :hero_image_url,
        :hero_image_alt,
        :summary,
        :body
      ]

      # hero_image_alt is optional for normal updates,
      # but this action requires it
      require_attributes [:hero_image_alt]
    end

    action :get_important_dates, {:array, :date} do
      run fn _input, _context ->
        {:ok, [~D[2025-01-15], ~D[2025-02-20], ~D[2025-03-25]]}
      end
    end

    action :get_publication_date, :date do
      run fn _input, _context ->
        {:ok, ~D[2025-01-15]}
      end
    end
  end
end
