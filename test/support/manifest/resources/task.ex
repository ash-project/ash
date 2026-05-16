# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Manifest.Task do
  @moduledoc """
  Test resource exercising attribute and argument names with unusual
  identifiers (trailing `?`). The manifest should surface them as-is
  in the IR.
  """

  use Ash.Resource,
    domain: Ash.Test.Manifest.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  attributes do
    uuid_primary_key :id

    attribute :title, :string do
      allow_nil? false
      public? true
    end

    attribute :completed, :boolean do
      allow_nil? false
      default false
      public? true
    end

    attribute :archived?, :boolean do
      allow_nil? false
      default false
      public? true
    end

    attribute :metadata, Ash.Test.Manifest.TaskMetadata do
      allow_nil? true
      public? true
    end

    attribute :stats, Ash.Test.Manifest.TaskStats do
      allow_nil? true
      public? true
    end

    attribute :custom_id, Ash.Test.Manifest.CustomIdentifier do
      allow_nil? true
      public? true
    end

    timestamps()
  end

  actions do
    defaults [:read]

    read :read_with_metadata do
      metadata :some_string, :string, allow_nil?: false, default: "default_value"
      metadata :some_number, :integer, allow_nil?: false, default: 123
      metadata :some_boolean, :boolean

      prepare fn query, _context ->
        Ash.Query.after_action(query, fn _query, results ->
          # Set metadata on each result
          results_with_metadata =
            Enum.map(results, fn record ->
              record
              |> Ash.Resource.put_metadata(:some_string, "default_value")
              |> Ash.Resource.put_metadata(:some_number, 123)
              |> Ash.Resource.put_metadata(:title, 1)
            end)

          {:ok, results_with_metadata}
        end)
      end
    end

    read :read_with_invalid_metadata_names do
      metadata :meta_1, :string, allow_nil?: false, default: "metadata_value"
      metadata :is_valid?, :boolean, allow_nil?: false, default: true
      metadata :field_2, :integer, allow_nil?: false, default: 999

      prepare fn query, _context ->
        Ash.Query.after_action(query, fn _query, results ->
          # Set metadata on each result
          results_with_metadata =
            Enum.map(results, fn record ->
              record
              |> Ash.Resource.put_metadata(:meta_1, "metadata_value")
              |> Ash.Resource.put_metadata(:is_valid?, true)
              |> Ash.Resource.put_metadata(:field_2, 999)
            end)

          {:ok, results_with_metadata}
        end)
      end
    end

    create :create do
      accept [:title]
      primary? true
      metadata :some_string, :string, allow_nil?: false
      metadata :some_number, :integer, allow_nil?: false
      metadata :some_boolean, :boolean

      change fn changeset, _context ->
        Ash.Changeset.after_action(changeset, fn _changeset, record ->
          {:ok,
           record
           |> Ash.Resource.put_metadata(:some_string, "created")
           |> Ash.Resource.put_metadata(:some_number, 456)
           |> Ash.Resource.put_metadata(:some_boolean, false)}
        end)
      end
    end

    update :update do
      accept [:title, :archived?, :stats]
      primary? true
      require_atomic? false
      metadata :some_string, :string, allow_nil?: false
      metadata :some_number, :integer, allow_nil?: false
      metadata :some_boolean, :boolean

      change fn changeset, _context ->
        Ash.Changeset.after_action(changeset, fn _changeset, record ->
          {:ok,
           record
           |> Ash.Resource.put_metadata(:some_string, "updated")
           |> Ash.Resource.put_metadata(:some_number, 789)
           |> Ash.Resource.put_metadata(:some_boolean, true)}
        end)
      end
    end

    update :mark_completed do
      require_atomic? false
      metadata :some_string, :string, allow_nil?: false
      metadata :some_number, :integer, allow_nil?: false
      metadata :some_boolean, :boolean

      argument :is_task_completed_now?, :boolean do
        allow_nil? false
      end

      change fn changeset, _context ->
        completed_value = Ash.Changeset.get_argument(changeset, :is_task_completed_now?)

        Ash.Changeset.change_attribute(changeset, :completed, completed_value)
        |> Ash.Changeset.after_action(fn _changeset, record ->
          {:ok,
           record
           |> Ash.Resource.put_metadata(:some_string, "some_value")
           |> Ash.Resource.put_metadata(:some_number, 123)
           |> Ash.Resource.put_metadata(:some_boolean, true)}
        end)
      end
    end

    destroy :destroy do
      accept [:archived?]
      primary? true
      require_atomic? false
      metadata :some_string, :string, allow_nil?: false
      metadata :some_number, :integer, allow_nil?: false
      metadata :some_boolean, :boolean

      manual fn changeset, _context ->
        # Manual destroy to ensure we can return the record with metadata
        record = changeset.data

        # Perform the actual deletion using the data layer
        with :ok <- Ash.DataLayer.destroy(changeset.resource, changeset) do
          # Create a struct with metadata to return
          record_with_metadata =
            record
            |> Ash.Resource.put_metadata(:some_string, "destroyed")
            |> Ash.Resource.put_metadata(:some_number, 999)
            |> Ash.Resource.put_metadata(:some_boolean, nil)

          {:ok, record_with_metadata}
        end
      end
    end

    action :get_task_stats, Ash.Test.Manifest.TaskStats do
      argument :task_id, :uuid, allow_nil?: false

      run fn input, _context ->
        # Simulate returning task statistics
        stats = %Ash.Test.Manifest.TaskStats{
          total_count: 10,
          completed?: true,
          is_urgent?: false,
          average_duration: 45.5
        }

        {:ok, stats}
      end
    end

    action :list_task_stats, {:array, Ash.Test.Manifest.TaskStats} do
      run fn _input, _context ->
        # Simulate returning multiple task statistics
        stats_list = [
          %Ash.Test.Manifest.TaskStats{
            total_count: 10,
            completed?: true,
            is_urgent?: false,
            average_duration: 45.5
          },
          %Ash.Test.Manifest.TaskStats{
            total_count: 5,
            completed?: false,
            is_urgent?: true,
            average_duration: 30.0
          }
        ]

        {:ok, stats_list}
      end
    end
  end
end
