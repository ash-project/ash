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
  end

  relationships do
    belongs_to :author, Ash.Test.Manifest.User, public?: true

    has_many :comments, Ash.Test.Manifest.PostComment,
      destination_attribute: :post_id,
      public?: true
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end
end
