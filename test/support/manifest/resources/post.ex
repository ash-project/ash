# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Post do
  @moduledoc """
  Test resource representing blog posts with comments.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id
    attribute :title, :string, allow_nil?: false, public?: true
    attribute :content, :string, public?: true
    attribute :published, :boolean, default: false, public?: true
    attribute :view_count, :integer, default: 0, public?: true
    attribute :rating, :decimal, public?: true
    attribute :published_at, :utc_datetime, public?: true
    attribute :tags, {:array, :string}, public?: true

    attribute :status, :atom do
      constraints one_of: [:draft, :published, :archived]
      public? true
    end

    attribute :metadata, :map, public?: true

    attribute :internal_code, Ash.Test.Manifest.Types.EmailString do
      public? false
    end
  end

  relationships do
    belongs_to :author, Ash.Test.Manifest.User, public?: true

    has_many :comments, Ash.Test.Manifest.PostComment,
      destination_attribute: :post_id,
      public?: true
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    update :update_internal_code do
      accept [:internal_code]

      metadata :preferences, Ash.Test.Manifest.InputParsing.PreferencesKeyword do
        allow_nil? false
      end
    end

    action :get_custom_metadata, Ash.Test.Manifest.InputParsing.Options do
      argument :metadata, Ash.Test.Manifest.CustomMetadata, allow_nil?: false

      run fn _input, _context ->
        {:ok, %{cache_enabled_1?: true, retry_limit: 3}}
      end
    end
  end
end
