# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoComment do
  @moduledoc """
  Test resource representing comments on todo items.
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

    attribute :content, :string do
      allow_nil? false
      public? true
    end

    attribute :author_name, :string do
      allow_nil? false
      public? true
    end

    attribute :rating, :integer do
      constraints min: 1, max: 5
      public? true
    end

    attribute :is_helpful, :boolean do
      default false
      public? true
    end

    # Embedded resource attribute for testing first aggregates with embedded types
    attribute :comment_metadata, Ash.Test.Manifest.TodoMetadata do
      public? true
    end

    # Union attribute with both embedded resource and regular Ash resource members
    # for testing first aggregates with union types
    attribute :author_info, :union do
      public? true

      constraints types: [
                    # Embedded resource member
                    metadata: [
                      type: Ash.Test.Manifest.TodoMetadata,
                      tag: :info_type,
                      tag_value: "metadata"
                    ],
                    # Simple type member
                    anonymous: [
                      type: :string
                    ]
                  ]
    end

    create_timestamp :created_at
    update_timestamp :updated_at
  end

  calculations do
    # A weighted score calculation for testing sum aggregates over calculations
    calculate :weighted_score, :integer, expr(rating * if(is_helpful, 2, 1)) do
      public? true
    end
  end

  relationships do
    belongs_to :todo, Ash.Test.Manifest.Todo do
      allow_nil? false
      public? true
    end

    belongs_to :user, Ash.Test.Manifest.User do
      allow_nil? false
      public? true
    end
  end

  actions do
    defaults [:read, :destroy]

    create :create do
      accept [:content, :author_name, :rating, :is_helpful, :comment_metadata, :author_info]

      argument :user_id, :uuid do
        allow_nil? false
        public? true
      end

      argument :todo_id, :uuid do
        allow_nil? false
        public? true
      end

      change manage_relationship(:user_id, :user, type: :append)
      change manage_relationship(:todo_id, :todo, type: :append)
    end

    update :update do
      require_atomic? false
      accept [:content, :author_name, :rating, :is_helpful, :comment_metadata, :author_info]
    end
  end
end
