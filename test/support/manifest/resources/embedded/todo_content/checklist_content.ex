# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.TodoContent.ChecklistContent do
  @moduledoc """
  Test embedded resource for checklist content (union type member).
  """
  use Ash.Resource,
    data_layer: :embedded,
    domain: nil

  attributes do
    uuid_primary_key :id

    attribute :title, :string, public?: true, allow_nil?: false

    attribute :items, {:array, :map},
      public?: true,
      default: [],
      constraints: [
        items: [
          fields: [
            text: [type: :string, allow_nil?: false],
            completed: [type: :boolean],
            created_at: [type: :utc_datetime]
          ]
        ]
      ]

    attribute :is_active?, :boolean, public?: true, default: true

    attribute :allow_reordering, :boolean, public?: true, default: true
    attribute :content_type, :string, public?: true, default: "checklist"
  end

  calculations do
    calculate :total_items, :integer, expr(length(items)) do
      public? true
    end

    calculate :completed_count, :integer, expr(0) do
      # In a real implementation, this would count completed items
      public? true
    end

    calculate :progress_percentage, :float, expr(0.0) do
      # In a real implementation, this would calculate percentage
      public? true
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    read :read_with_arg do
      argument :is_active?, :boolean
    end

    create :create do
      primary? true
      accept [:title, :items, :allow_reordering]
    end
  end
end
