# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.Changes.CascadeParentFilterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Task do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, :destroy, create: [:id, :org_id, :project_id, :reviewer_id]]

      update :archive do
        require_atomic? false
        change set_attribute(:archived?, true)
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
      attribute :project_id, :integer, public?: true
      attribute :reviewer_id, :integer, public?: true
      attribute :archived?, :boolean, default: false, public?: true
    end
  end

  defmodule Project do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:id, :org_id, :reviewer_id]]

      destroy :destroy do
        primary? true
        change cascade_destroy(:tasks, action: :destroy)
      end

      destroy :destroy_reviewed do
        change cascade_destroy(:reviewed_tasks, action: :destroy)
      end

      update :archive do
        require_atomic? false
        change cascade_update(:tasks, action: :archive)
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
      attribute :reviewer_id, :integer, public?: true
    end

    relationships do
      has_many :tasks, Task do
        destination_attribute :project_id
        filter expr(org_id == parent(org_id))
        public? true
      end

      has_many :reviewed_tasks, Task do
        destination_attribute :project_id
        filter expr(reviewer_id == parent(reviewer.id))
        public? true
      end

      belongs_to :reviewer, Task do
        allow_nil? true
        attribute_type :integer
        define_attribute? false
        source_attribute :reviewer_id
        public? true
      end
    end
  end

  setup do
    project =
      Project
      |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10})
      |> Ash.create!()

    other_project =
      Project
      |> Ash.Changeset.for_create(:create, %{id: 2, org_id: 20})
      |> Ash.create!()

    for {id, org_id, project_id} <- [{1, 10, 1}, {2, 20, 1}, {3, 20, 2}] do
      Task
      |> Ash.Changeset.for_create(:create, %{id: id, org_id: org_id, project_id: project_id})
      |> Ash.create!()
    end

    %{project: project, other_project: other_project}
  end

  defp task_ids do
    Task |> Ash.read!() |> Enum.map(& &1.id) |> Enum.sort()
  end

  describe "cascade_destroy" do
    test "destroys only the rows the parent()-scoped relationship contains", %{
      project: project
    } do
      assert :ok = Ash.destroy(project)
      assert task_ids() == [2, 3]
    end

    test "resolves parent() per record on a bulk destroy", %{
      project: project,
      other_project: other_project
    } do
      assert %Ash.BulkResult{status: :success} =
               Ash.bulk_destroy!([project, other_project], :destroy, %{},
                 strategy: :stream,
                 return_errors?: true
               )

      assert task_ids() == [2]
    end

    test "fails loudly when parent() cannot be resolved from the record", %{project: project} do
      assert {:error, error} = Ash.destroy(project, action: :destroy_reviewed)
      assert Exception.message(error) =~ "parent"
    end
  end

  describe "cascade_update" do
    test "updates only the rows the parent()-scoped relationship contains", %{
      project: project
    } do
      assert {:ok, _} = project |> Ash.Changeset.for_update(:archive, %{}) |> Ash.update()

      assert Task |> Ash.read!() |> Enum.filter(& &1.archived?) |> Enum.map(& &1.id) == [1]
    end
  end
end
