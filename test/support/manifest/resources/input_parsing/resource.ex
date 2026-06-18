# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.InputParsing.Resource do
  @moduledoc """
  Stress-test resource exercising the manifest type resolver on edge cases.

  Exercises:
  1. Plain attributes
  2. Attribute names with unusual characters (trailing `?`, trailing `_N`)
  3. Arguments with unusual names
  4. Nested embedded resources with various field shapes
  5. NewTypes with constrained map/keyword/tuple fields
  6. Union types with various member shapes
  7. Deeply nested typed maps (outer → inner)
  8. 3-level embedded resource nesting (Resource → NestedProfile → Profile)
  9. Union with `:map_with_tag` storage
  10. Array of unions with embedded resource members
  11. Generic action with embedded resource argument
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

    # Plain attributes
    attribute :user_name, :string do
      allow_nil? false
      public? true
    end

    attribute :email_address, :string do
      allow_nil? false
      public? true
    end

    # Additional attribute
    attribute :display_name, :string do
      allow_nil? true
      public? true
    end

    # Attributes with unusual identifiers (trailing `?`, trailing `_N`)
    attribute :is_active?, :boolean do
      default true
      public? true
    end

    attribute :has_data?, :boolean do
      default false
      public? true
    end

    attribute :version_1, :integer do
      default 1
      public? true
    end

    # Typed map with field constraints
    attribute :settings, :map do
      public? true

      constraints fields: [
                    notification_enabled: [type: :boolean, allow_nil?: false],
                    theme_name: [type: :string, allow_nil?: true],
                    retry_count: [type: :integer, allow_nil?: true]
                  ]
    end

    # Typed map using a NewType with constrained fields
    attribute :stats, Ash.Test.Manifest.InputParsing.Stats do
      public? true
    end

    # Embedded resource attribute
    attribute :profile_data, Ash.Test.Manifest.InputParsing.Profile do
      public? true
    end

    # Array of embedded resources
    attribute :history, {:array, Ash.Test.Manifest.InputParsing.HistoryEntry} do
      default []
      public? true
    end

    # Union type with embedded resources and NewType map member
    attribute :content, :union do
      public? true

      constraints types: [
                    # Embedded resource member
                    text: [
                      type: Ash.Test.Manifest.InputParsing.TextContent,
                      tag: :content_type,
                      tag_value: "text"
                    ],
                    # NewType map member with constrained fields
                    data: [
                      type: Ash.Test.Manifest.InputParsing.DataContentMap,
                      tag: :content_type,
                      tag_value: "data"
                    ],
                    # Simple type member (no special handling needed)
                    simple_value: [type: :string]
                  ],
                  storage: :type_and_value
    end

    # =========================================================================
    # Additional attributes for exhaustive type resolver coverage
    # =========================================================================

    # Tuple type with constrained fields
    attribute :location, Ash.Test.Manifest.InputParsing.LocationTuple do
      public? true
    end

    # Keyword type with constrained fields
    attribute :preferences, Ash.Test.Manifest.InputParsing.PreferencesKeyword do
      public? true
    end

    # Deeply nested typed maps
    attribute :deep_settings, Ash.Test.Manifest.InputParsing.DeepNestedSettings do
      public? true
    end

    # 3-level embedded resource nesting
    attribute :nested_profile, Ash.Test.Manifest.InputParsing.NestedProfile do
      public? true
    end

    # Union with :map_with_tag storage (different from :type_and_value)
    # Exercises union resolution for :map_with_tag storage
    # Note: map_with_tag requires ALL members to have tags
    attribute :tagged_status, :union do
      public? true

      constraints types: [
                    active: [
                      type: Ash.Test.Manifest.InputParsing.TaggedStatus,
                      tag: :status_type,
                      tag_value: "active"
                    ],
                    inactive: [
                      type: Ash.Test.Manifest.InputParsing.TaggedStatus,
                      tag: :status_type,
                      tag_value: "inactive"
                    ]
                  ],
                  storage: :map_with_tag
    end

    # Array of unions with embedded resource members
    # Tests array union input
    attribute :attachments, {:array, :union} do
      public? true
      default []

      constraints items: [
                    types: [
                      file: [
                        type: Ash.Test.Manifest.InputParsing.AttachmentFile,
                        tag: :attachment_type,
                        tag_value: "file"
                      ],
                      link: [
                        type: Ash.Test.Manifest.InputParsing.AttachmentLink,
                        tag: :attachment_type,
                        tag_value: "link"
                      ],
                      # Simple string member for comparison
                      note: [type: :string]
                    ]
                  ]
    end

    create_timestamp :created_at do
      public? true
    end

    update_timestamp :updated_at
  end

  actions do
    defaults [:destroy]

    read :read do
      primary? true
    end

    read :get_by_id do
      get_by :id
    end

    # Search action with mapped argument names
    read :search do
      argument :query, :string, allow_nil?: false

      argument :include_deleted?, :boolean do
        default false
      end

      argument :filter_by_1?, :boolean do
        default false
      end

      filter expr(contains(user_name, ^arg(:query)) or contains(email_address, ^arg(:query)))
    end

    create :create do
      primary? true

      accept [
        :user_name,
        :email_address,
        :display_name,
        :is_active?,
        :has_data?,
        :version_1,
        :settings,
        :stats,
        :profile_data,
        :history,
        :content,
        # New attributes for exhaustive coverage
        :location,
        :preferences,
        :deep_settings,
        :nested_profile,
        :tagged_status,
        :attachments
      ]
    end

    # Create action with mapped argument names
    create :create_with_args do
      accept [
        :user_name,
        :email_address,
        :is_active?,
        :has_data?,
        :settings
      ]

      argument :is_urgent?, :boolean do
        default false
      end

      argument :priority_1, :integer do
        default 1
      end

      argument :extra_data, :map do
        allow_nil? true
      end
    end

    update :update do
      primary? true
      require_atomic? false

      accept [
        :user_name,
        :email_address,
        :display_name,
        :is_active?,
        :has_data?,
        :version_1,
        :settings,
        :stats,
        :profile_data,
        :history,
        :content,
        # New attributes for exhaustive coverage
        :location,
        :preferences,
        :deep_settings,
        :nested_profile,
        :tagged_status,
        :attachments
      ]
    end

    # Generic action to test input/output with NewType argument
    action :process_data, :map do
      constraints fields: [
                    processed: [type: :boolean, allow_nil?: false],
                    result_count: [type: :integer, allow_nil?: false]
                  ]

      # Uses NewType with constrained fields
      argument :input_data, Ash.Test.Manifest.InputParsing.InputDataMap do
        allow_nil? false
      end

      # Uses NewType with constrained fields
      argument :options, Ash.Test.Manifest.InputParsing.Options do
        allow_nil? true
      end

      run fn input, _context ->
        {:ok,
         %{
           processed: true,
           result_count: 1
         }}
      end
    end

    # Generic action with embedded resource argument
    # Action argument is a full embedded resource
    action :process_profile, Ash.Test.Manifest.InputParsing.ProcessProfileResult do
      # Embedded resource as action argument
      argument :profile, Ash.Test.Manifest.InputParsing.Profile do
        allow_nil? false
      end

      # Optional nested profile for deeper nesting test
      argument :nested, Ash.Test.Manifest.InputParsing.NestedProfile do
        allow_nil? true
      end

      run fn input, _context ->
        profile = input.arguments.profile

        {:ok,
         %{
           profile_name: profile.display_name,
           is_processed?: true
         }}
      end
    end
  end
end
