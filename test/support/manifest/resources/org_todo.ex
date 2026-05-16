# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.OrgTodo do
  @moduledoc """
  Test resource for organization-level todos with multitenancy support.
  """
  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets,
    primary_read_warning?: false

  ets do
    private? true
  end

  multitenancy do
    strategy :context
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

    attribute :tags, {:array, :string} do
      default []
      public? true
    end

    attribute :metadata, :map do
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
  end

  aggregates do
    # Aggregates removed as they referenced comment relationships
    # which are not applicable for the simplified OrgTodo resource
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

      # Private argument for internal use - should NOT appear in the manifest
      argument :internal_audit_mode, :boolean do
        public? false
        default false
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
    end

    read :get_by_id do
      get_by [:id]
    end

    create :create do
      primary? true
      accept [:title, :description, :status, :priority, :due_date, :tags, :metadata]

      argument :auto_complete, :boolean do
        default false
      end

      argument :user_id, :uuid do
        allow_nil? false
      end

      # Private argument for internal tracking - should NOT appear in the manifest
      argument :internal_tracking_id, :string do
        public? false
        default nil
      end

      argument :number_of_employees, :integer do
        allow_nil? false
        constraints min: 1, max: 1000
      end

      argument :some_string, :string do
        allow_nil? false
        constraints min_length: 1, max_length: 100
      end

      # Regex constraint tests - various patterns
      argument :email, :string do
        allow_nil? false
        constraints match: ~r/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/
      end

      argument :phone_number, :string do
        allow_nil? false
        constraints match: ~r/^\+?[1-9]\d{1,14}$/
      end

      argument :hex_color, :string do
        allow_nil? false
        constraints match: ~r/^#[0-9A-Fa-f]{6}$/
      end

      argument :slug, :string do
        allow_nil? false
        constraints match: ~r/^[a-z0-9]+(?:-[a-z0-9]+)*$/
      end

      argument :version, :string do
        allow_nil? false
        constraints match: ~r/^\d+\.\d+\.\d+$/
      end

      argument :case_insensitive_code, :string do
        allow_nil? false
        constraints match: ~r/^[A-Z]{3}-\d{4}$/i
      end

      argument :optional_url, :string do
        allow_nil? true
        constraints match: ~r/^https?:\/\/.+/
      end

      # Float constraint tests
      argument :price, :float do
        allow_nil? false
        constraints min: 0.0, max: 999_999.99
      end

      argument :temperature, :float do
        allow_nil? false
        constraints greater_than: -273.15, less_than: 1_000_000.0
      end

      argument :percentage, :float do
        allow_nil? false
        constraints min: 0.0, max: 100.0
      end

      argument :optional_rating, :float do
        allow_nil? true
        constraints min: 0.0, max: 5.0
      end

      # CiString constraint tests
      argument :username, :ci_string do
        allow_nil? false
        constraints min_length: 3, max_length: 20
      end

      argument :company_name, :ci_string do
        allow_nil? false
        constraints min_length: 2, max_length: 100, match: ~r/^[a-zA-Z0-9\s]+$/
      end

      argument :country_code, :ci_string do
        allow_nil? false
        constraints match: ~r/^[A-Z]{2}$/i
      end

      argument :optional_nickname, :ci_string do
        allow_nil? true
        constraints min_length: 2, max_length: 15
      end

      argument :address, :map do
        constraints fields: [
                      street_address: [
                        type: :string,
                        allow_nil?: false,
                        description: "Street Address is required",
                        constraints: [min_length: 5]
                      ],
                      location_id: [
                        type: :string,
                        allow_nil?: false,
                        description: "Postal code is required",
                        constraints: [min_length: 5]
                      ],
                      details: [type: :string, allow_nil?: true]
                    ]

        allow_nil? false
      end

      change set_attribute(:completed, arg(:auto_complete))
      change manage_relationship(:user_id, :user, type: :append)
    end

    update :update do
      primary? true
      accept [:title, :description, :completed, :status, :priority, :due_date, :tags, :metadata]
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

      # Private argument for internal use - should NOT appear in the manifest
      argument :bypass_validation, :boolean do
        public? false
        default false
      end

      change set_attribute(:priority, arg(:priority))
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

      # Private argument for internal use - should NOT appear in the manifest
      argument :debug_mode, :boolean do
        public? false
        default false
      end

      run fn _input, _context ->
        # This would normally search todos, but for testing we'll return empty
        {:ok, []}
      end
    end
  end
end
