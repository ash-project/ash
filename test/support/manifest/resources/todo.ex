# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Todo do
  @moduledoc """
  Test resource representing a todo item with relationships and calculations.
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

    attribute :title, :string do
      allow_nil? false
      public? true
    end

    attribute :description, :string do
      public? true
    end

    attribute :completed, :boolean do
      default false
      public? true
    end

    attribute :status, Ash.Test.Manifest.Todo.Status do
      default :pending
      public? true
    end

    attribute :priority, :atom do
      constraints one_of: [:low, :medium, :high, :urgent]
      default :medium
      public? true
    end

    attribute :due_date, :date do
      public? true
    end

    attribute :priority_score, Ash.Test.Manifest.Todo.PriorityScore do
      public? true
    end

    attribute :color_palette, Ash.Test.Manifest.Todo.ColorPalette do
      public? true
    end

    attribute :percentage, Ash.Test.Manifest.Todo.Percentage do
      public? true
    end

    attribute :tags, {:array, :string} do
      default []
      public? true
    end

    attribute :metadata, Ash.Test.Manifest.TodoMetadata do
      public? true
    end

    attribute :metadata_history, {:array, Ash.Test.Manifest.TodoMetadata} do
      default []
      public? true
    end

    # Union type attribute demonstrating tagged union with embedded resources
    attribute :content, :union do
      public? true

      constraints types: [
                    text: [
                      type: Ash.Test.Manifest.TodoContent.TextContent,
                      tag: :content_type,
                      tag_value: "text"
                    ],
                    checklist: [
                      type: Ash.Test.Manifest.TodoContent.ChecklistContent,
                      tag: :content_type,
                      tag_value: "checklist"
                    ],
                    link: [
                      type: Ash.Test.Manifest.TodoContent.LinkContent,
                      tag: :content_type,
                      tag_value: "link"
                    ],
                    # Simple types for testing untagged unions
                    note: [
                      type: :string
                    ],
                    priority_value: [
                      type: :integer,
                      constraints: [min: 1, max: 10]
                    ]
                  ],
                  storage: :type_and_value
    end

    # Union type array for testing array union support
    attribute :attachments, {:array, :union} do
      public? true
      default []

      constraints items: [
                    types: [
                      file: [
                        type: :map,
                        tag: :attachment_type,
                        tag_value: "file",
                        constraints: [
                          fields: [
                            filename: [type: :string, allow_nil?: false],
                            size: [type: :integer],
                            mime_type: [type: :string]
                          ]
                        ]
                      ],
                      image: [
                        type: :map,
                        tag: :attachment_type,
                        tag_value: "image",
                        constraints: [
                          fields: [
                            filename: [type: :string, allow_nil?: false],
                            width: [type: :integer],
                            height: [type: :integer],
                            alt_text: [type: :string]
                          ]
                        ]
                      ],
                      # Simple untagged union member
                      url: [
                        type: :string,
                        constraints: [match: ~r/^https?:\/\//]
                      ]
                    ]
                  ]
    end

    # Union type with :map_with_tag storage for testing alternative storage mode
    attribute :status_info, :union do
      public? true

      constraints types: [
                    simple: [
                      type: :map,
                      tag: :status_type,
                      tag_value: "simple"
                    ],
                    detailed: [
                      type: :map,
                      tag: :status_type,
                      tag_value: "detailed"
                    ],
                    automated: [
                      type: :map,
                      tag: :status_type,
                      tag_value: "automated"
                    ]
                  ],
                  storage: :map_with_tag
    end

    attribute :timestamp_info, Ash.Test.Manifest.TodoTimestamp do
      public? true
    end

    attribute :statistics, Ash.Test.Manifest.TodoStatistics do
      public? true
    end

    attribute :options, :keyword do
      public? true
      allow_nil? true

      constraints fields: [
                    priority: [
                      type: :integer,
                      allow_nil?: false,
                      description: "Priority level (1-10)",
                      constraints: [min: 1, max: 10]
                    ],
                    category: [
                      type: :string,
                      allow_nil?: true,
                      description: "Todo category",
                      constraints: [max_length: 50]
                    ],
                    notify: [
                      type: :boolean,
                      allow_nil?: true,
                      description: "Whether to send notifications"
                    ]
                  ]
    end

    attribute :coordinates, :tuple do
      public? true

      constraints fields: [
                    latitude: [
                      type: :float,
                      allow_nil?: false,
                      description: "Latitude coordinate",
                      constraints: [min: -90.0, max: 90.0]
                    ],
                    longitude: [
                      type: :float,
                      allow_nil?: false,
                      description: "Longitude coordinate",
                      constraints: [min: -180.0, max: 180.0]
                    ]
                  ]
    end

    # Untyped map attribute for testing untyped map support
    attribute :custom_data, :map do
      public? true
    end

    create_timestamp :created_at do
      public? true
    end

    update_timestamp :updated_at
  end

  relationships do
    belongs_to :user, Ash.Test.Manifest.User do
      allow_nil? false
      public? true
    end

    has_many :comments, Ash.Test.Manifest.TodoComment do
      public? true
    end

    has_many :not_exposed_items, Ash.Test.Manifest.NotExposed do
      public? true
    end
  end

  aggregates do
    count :comment_count, :comments do
      public? true
    end

    count :helpful_comment_count, :comments do
      public? true
      filter expr(is_helpful == true)
    end

    exists :has_comments, :comments do
      public? true
    end

    avg :average_rating, :comments, :rating do
      public? true
    end

    max :highest_rating, :comments, :rating do
      public? true
    end

    first :latest_comment_content, :comments, :content do
      public? true
      sort created_at: :desc
    end

    list :comment_authors, :comments, :author_name do
      public? true
    end

    # Additional field-based aggregates
    first :latest_comment_id, :comments, :id do
      public? true
      sort created_at: :desc
    end

    list :recent_comment_ids, :comments, :id do
      public? true
      sort created_at: :desc
    end

    # Aggregates over calculation fields (not attributes)
    # These test the fix that allows aggregates to reference calculation fields
    sum :total_weighted_score, :comments, :weighted_score do
      public? true
    end

    list :weighted_scores, :comments, :weighted_score do
      public? true
    end

    first :first_weighted_score, :comments, :weighted_score do
      public? true
    end

    max :max_weighted_score, :comments, :weighted_score do
      public? true
    end

    # First aggregates that return complex types for testing
    # First aggregate returning an embedded resource
    first :first_comment_metadata, :comments, :comment_metadata do
      public? true
      sort created_at: :desc
    end

    # First aggregate returning a union type
    first :first_comment_author_info, :comments, :author_info do
      public? true
      sort created_at: :desc
    end
  end

  calculations do
    calculate :is_overdue, :boolean, Ash.Test.Manifest.IsOverdueCalculation do
      public? true
    end

    calculate :days_until_due, :integer, Ash.Test.Manifest.Todo.SimpleDateCalculation do
      public? true
    end

    calculate :self, :struct, Ash.Test.Manifest.SelfCalculation do
      constraints instance_of: __MODULE__
      public? true

      argument :prefix, :string do
        allow_nil? true
        default nil
      end

      argument :count, :integer do
        allow_nil? true
        default nil
      end

      argument :enabled, :boolean do
        allow_nil? true
        default nil
      end

      argument :data, :map do
        allow_nil? true
        default nil
      end
    end

    calculate :summary,
              Ash.Test.Manifest.TodoStatistics,
              Ash.Test.Manifest.SummaryCalculation do
      public? true
    end

    # Calculation with arguments that use types requiring type aliases
    # This tests that calculation argument types are discovered for type alias generation
    calculate :filtered_data, :string, expr("filtered") do
      public? true

      argument :after_date, Ash.Type.Date do
        allow_nil? true
        default nil
      end

      argument :user_id, Ash.Type.UUID do
        allow_nil? true
        default nil
      end
    end
  end

  actions do
    defaults [:destroy]

    read :read do
      primary? true
      argument :filter_completed, :boolean

      argument :priority_filter, :atom do
        constraints one_of: [:low, :medium, :high, :urgent]
      end

      filter expr(
               if is_nil(^arg(:filter_completed)) do
                 true
               else
                 completed == ^arg(:filter_completed)
               end and
                 if is_nil(^arg(:priority_filter)) do
                   true
                 else
                   priority == ^arg(:priority_filter)
                 end
             )

      pagination offset?: true,
                 keyset?: true,
                 countable: true,
                 required?: false,
                 default_limit: 20,
                 max_page_size: 100
    end

    read :get_by_id do
      get_by :id
    end

    create :create do
      primary? true

      accept [
        :title,
        :description,
        :status,
        :priority,
        :due_date,
        :tags,
        :metadata,
        :metadata_history,
        :content,
        :attachments,
        :status_info,
        :priority_score,
        :color_palette,
        :timestamp_info,
        :statistics,
        :options,
        :coordinates,
        :custom_data
      ]

      argument :auto_complete, :boolean do
        default false
      end

      argument :user_id, :uuid do
        allow_nil? false
      end

      change set_attribute(:completed, arg(:auto_complete))
      change manage_relationship(:user_id, :user, type: :append)
    end

    update :update do
      primary? true
      require_atomic? false

      accept [
        :title,
        :description,
        :completed,
        :status,
        :priority,
        :due_date,
        :tags,
        :metadata,
        :content,
        :attachments,
        :status_info,
        :priority_score,
        :color_palette,
        :timestamp_info,
        :statistics,
        :options,
        :coordinates,
        :custom_data
      ]
    end

    update :complete do
      accept []
      change set_attribute(:completed, true)
    end

    update :set_priority do
      argument :priority, :atom do
        allow_nil? false
        constraints one_of: [:low, :medium, :high, :urgent]
      end

      change set_attribute(:priority, arg(:priority))
    end

    update :update_with_untyped_data do
      require_atomic? false
      accept [:custom_data]

      argument :additional_data, :map do
        allow_nil? false
      end

      argument :metadata_update, :map do
        allow_nil? true
      end

      change fn changeset, _context ->
        # Merge the argument data with the custom_data attribute
        additional_data = Ash.Changeset.get_argument(changeset, :additional_data)
        metadata_update = Ash.Changeset.get_argument(changeset, :metadata_update)

        current_custom_data = Ash.Changeset.get_data(changeset, :custom_data) || %{}

        merged_data = Map.merge(current_custom_data, additional_data)

        merged_data =
          if metadata_update,
            do: Map.put(merged_data, "metadataUpdate", metadata_update),
            else: merged_data

        Ash.Changeset.change_attribute(changeset, :custom_data, merged_data)
      end
    end

    action :bulk_complete, {:array, :uuid} do
      argument :todo_ids, {:array, :uuid}, allow_nil?: false

      run fn input, _context ->
        # This would normally update multiple todos, but for testing we'll just return the IDs
        {:ok, input.arguments.todo_ids}
      end
    end

    action :get_statistics, :map do
      constraints fields: [
                    total: [type: :integer, allow_nil?: false],
                    completed: [type: :integer, allow_nil?: false],
                    pending: [type: :integer, allow_nil?: false],
                    overdue: [type: :integer, allow_nil?: false]
                  ]

      run fn _input, _context ->
        {:ok,
         %{
           total: 10,
           completed: 6,
           pending: 4,
           overdue: 2
         }}
      end
    end

    action :search, {:array, Ash.Type.Struct} do
      constraints items: [instance_of: __MODULE__]

      argument :query, :string, allow_nil?: false
      argument :include_completed, :boolean, default: true

      run fn _input, _context ->
        # This would normally search todos, but for testing we'll return empty
        {:ok, []}
      end
    end

    action :get_keyword_options, :keyword do
      constraints fields: [
                    priority: [
                      type: :integer,
                      allow_nil?: false,
                      description: "Priority level (1-10)",
                      constraints: [min: 1, max: 10]
                    ],
                    category: [
                      type: :string,
                      allow_nil?: false,
                      description: "Todo category",
                      constraints: [max_length: 50]
                    ],
                    notify: [
                      type: :boolean,
                      allow_nil?: false,
                      description: "Whether to send notifications"
                    ],
                    theme: [
                      type: :atom,
                      allow_nil?: false,
                      description: "UI theme preference",
                      constraints: [one_of: [:light, :dark, :auto]]
                    ]
                  ]

      run fn _input, _context ->
        {:ok,
         [
           priority: 8,
           category: "work",
           notify: true,
           theme: :dark
         ]}
      end
    end

    action :get_coordinates_info, :tuple do
      constraints fields: [
                    latitude: [
                      type: :float,
                      allow_nil?: false,
                      description: "Latitude coordinate",
                      constraints: [min: -90.0, max: 90.0]
                    ],
                    longitude: [
                      type: :float,
                      allow_nil?: false,
                      description: "Longitude coordinate",
                      constraints: [min: -180.0, max: 180.0]
                    ],
                    altitude: [
                      type: :integer,
                      allow_nil?: true,
                      description: "Altitude in meters"
                    ]
                  ]

      run fn _input, _context ->
        {:ok, {37.7749, -122.4194, 50}}
      end
    end

    action :get_custom_data, :map do
      run fn _input, _context ->
        {:ok,
         %{
           user_id: "123e4567-e89b-12d3-a456-426614174000",
           status: "active",
           metadata: %{
             version: "1.0",
             tags: ["important", "urgent"],
             settings: %{
               notifications: true,
               theme: "dark"
             }
           },
           count: 42,
           timestamp: 1_640_995_200
         }}
      end
    end

    action :get_custom_data_list, {:array, :map} do
      run fn _input, _context ->
        {:ok,
         [
           %{user_id: "123e4567-e89b-12d3-a456-426614174000", status: "active"},
           %{user_id: "223e4567-e89b-12d3-a456-426614174001", status: "pending"}
         ]}
      end
    end

    # Additional read action with different pagination configuration for testing
    read :search_paginated do
      argument :query, :string, allow_nil?: false
      argument :include_completed, :boolean, default: true

      filter expr(
               if is_nil(^arg(:query)) do
                 true
               else
                 contains(title, ^arg(:query)) or contains(description, ^arg(:query))
               end and
                 if ^arg(:include_completed) do
                   true
                 else
                   completed != true
                 end
             )

      pagination offset?: true,
                 keyset?: false,
                 countable: true,
                 required?: true,
                 default_limit: 10,
                 max_page_size: 50
    end

    # Read action with keyset-only pagination
    read :list_recent do
      filter expr(created_at >= ago(7, :day))

      pagination required?: false,
                 offset?: false,
                 keyset?: true,
                 countable: false,
                 default_limit: 25,
                 max_page_size: 100
    end

    # Read action with no pagination (should not have page field)
    read :list_high_priority do
      filter expr(priority in [:high, :urgent])
    end

    action :assign_to_user, :map do
      constraints fields: [
                    assignee_id: [type: :uuid, allow_nil?: false],
                    assignee_name: [type: :string, allow_nil?: false],
                    reason: [type: :string, allow_nil?: true]
                  ]

      argument :assignee, :struct do
        constraints instance_of: Ash.Test.Manifest.User
        allow_nil? false
      end

      argument :reason, :string do
        allow_nil? true
      end

      run fn input, _context ->
        assignee = input.arguments.assignee
        reason = Map.get(input.arguments, :reason)

        # Verify the assignee is actually a User struct
        if not is_struct(assignee, Ash.Test.Manifest.User) do
          raise "Expected assignee to be a User struct, got: #{inspect(assignee)}"
        end

        # Return assignment info
        {:ok,
         %{
           assignee_id: assignee.id,
           assignee_name: assignee.name,
           reason: reason
         }}
      end
    end

    # Action with an array of resource structs as argument
    action :assign_to_users, {:array, :map} do
      constraints items: [
                    fields: [
                      assignee_id: [type: :uuid, allow_nil?: false],
                      assignee_name: [type: :string, allow_nil?: false]
                    ]
                  ]

      argument :assignees, {:array, :struct} do
        constraints items: [instance_of: Ash.Test.Manifest.User]
        allow_nil? false
      end

      run fn input, _context ->
        assignees = input.arguments.assignees

        # Verify all assignees are User structs
        Enum.each(assignees, fn assignee ->
          if not is_struct(assignee, Ash.Test.Manifest.User) do
            raise "Expected assignee to be a User struct, got: #{inspect(assignee)}"
          end
        end)

        # Return assignment info for each assignee
        results =
          Enum.map(assignees, fn assignee ->
            %{
              assignee_id: assignee.id,
              assignee_name: assignee.name
            }
          end)

        {:ok, results}
      end
    end

    action :process_metadata, :map do
      constraints fields: [
                    processed: [type: :boolean, allow_nil?: false],
                    priority: [type: :integer, allow_nil?: true],
                    source: [type: :string, allow_nil?: false]
                  ]

      argument :metadata, Ash.Test.Manifest.TodoMetadata, allow_nil?: false

      argument :options, :map do
        allow_nil? true
      end

      run fn input, _context ->
        metadata = input.arguments.metadata

        {:ok,
         %{
           processed: true,
           priority: Map.get(metadata, :priority_score),
           source: "direct_embedded_argument"
         }}
      end
    end

    action :process_metadata_batch, {:array, :map} do
      constraints items: [
                    fields: [
                      processed: [type: :boolean, allow_nil?: false],
                      priority: [type: :integer, allow_nil?: true]
                    ]
                  ]

      argument :metadata_items, {:array, Ash.Test.Manifest.TodoMetadata}, allow_nil?: false

      run fn input, _context ->
        results =
          input.arguments.metadata_items
          |> Enum.map(fn metadata ->
            %{
              processed: true,
              priority: Map.get(metadata, :priority_score)
            }
          end)

        {:ok, results}
      end
    end

    # Private action — exercises the `:include_private_actions?` opt. Should
    # not appear in entrypoints by default.
    read :internal_reconcile do
      public? false
    end
  end
end
