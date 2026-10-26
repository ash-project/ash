# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ManageRelationshipParentFilterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Tag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:id, :org_id]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end
  end

  defmodule ProjectTag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, :destroy, create: :*]
    end

    attributes do
      attribute :project_id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :tag_id, :integer, primary_key?: true, allow_nil?: false, public?: true
    end
  end

  defmodule Employee do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:id, :org_id, :name, :project_id]]

      update :update do
        primary? true
        require_atomic? false
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
      attribute :name, :string, allow_nil?: false, public?: true
      attribute :project_id, :integer, public?: true
    end
  end

  defmodule Project do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:id, :org_id]]

      create :create_assigned do
        accept [:id, :org_id]
        argument :assignee_id, :integer, allow_nil?: false
        change manage_relationship(:assignee_id, :assignee, type: :append_and_remove)
      end

      update :assign do
        require_atomic? false
        argument :assignee_id, :integer, allow_nil?: false
        change manage_relationship(:assignee_id, :assignee, type: :append_and_remove)
      end

      update :assign_reviewer do
        require_atomic? false
        argument :reviewer_id, :integer, allow_nil?: false
        change manage_relationship(:reviewer_id, :reviewer, type: :append_and_remove)
      end

      update :add_employees do
        require_atomic? false
        argument :employee_ids, {:array, :integer}, allow_nil?: false
        change manage_relationship(:employee_ids, :employees, type: :append, value_is_key: :id)
      end

      update :bulk_add_employees do
        require_atomic? false
        argument :employees, {:array, :map}, allow_nil?: false

        change manage_relationship(:employees,
                 on_lookup: :relate,
                 on_no_match: :create,
                 bulk?: true
               )
      end

      update :tag do
        require_atomic? false
        argument :tag_ids, {:array, :integer}, allow_nil?: false
        change manage_relationship(:tag_ids, :tags, type: :append_and_remove, value_is_key: :id)
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :assignee, Employee do
        allow_nil? true
        attribute_type :integer
        filter expr(org_id == parent(org_id))
        public? true
      end

      belongs_to :reviewer, Employee do
        allow_nil? true
        attribute_type :integer
        filter expr(org_id == parent(assignee.org_id))
        public? true
      end

      has_many :employees, Employee do
        destination_attribute :project_id
        filter expr(org_id == parent(org_id))
        public? true
      end

      many_to_many :tags, Tag do
        through ProjectTag
        source_attribute_on_join_resource :project_id
        destination_attribute_on_join_resource :tag_id
        filter expr(org_id == parent(org_id))
        public? true
      end
    end
  end

  defmodule TenantEmployee do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
    end

    actions do
      defaults [:read, create: [:id]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end
  end

  defmodule TenantTag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
    end

    actions do
      defaults [:read, create: [:id]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end
  end

  defmodule TenantProjectTag do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
    end

    actions do
      defaults [:read, :destroy, create: :*]
    end

    attributes do
      attribute :project_id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :tag_id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end
  end

  defmodule TenantProject do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
    end

    actions do
      defaults [:read, create: [:id]]

      create :create_assigned do
        accept [:id]
        argument :assignee_id, :integer, allow_nil?: false
        change manage_relationship(:assignee_id, :assignee, type: :append_and_remove)
      end

      update :tag do
        require_atomic? false
        argument :tag_ids, {:array, :integer}, allow_nil?: false
        change manage_relationship(:tag_ids, :tags, type: :append_and_remove, value_is_key: :id)
      end
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
    end

    relationships do
      belongs_to :assignee, TenantEmployee do
        allow_nil? true
        attribute_type :integer
        filter expr(org_id == parent(org_id))
        public? true
      end

      many_to_many :tags, TenantTag do
        through TenantProjectTag
        join_relationship :project_tags
        source_attribute_on_join_resource :project_id
        destination_attribute_on_join_resource :tag_id
        filter expr(org_id == parent(org_id) and org_id == parent(project_tags.org_id))
        public? true
      end
    end
  end

  setup do
    same_org =
      Employee
      |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10, name: "same org"})
      |> Ash.create!()

    foreign =
      Employee
      |> Ash.Changeset.for_create(:create, %{id: 2, org_id: 20, name: "foreign org"})
      |> Ash.create!()

    project =
      Project
      |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10})
      |> Ash.create!()

    %{same_org: same_org, foreign: foreign, project: project}
  end

  describe "belongs_to lookup" do
    test "relates a record that satisfies the parent()-scoped filter", %{
      project: project,
      same_org: same_org
    } do
      assert {:ok, %{assignee_id: 1}} =
               project
               |> Ash.Changeset.for_update(:assign, %{assignee_id: same_org.id})
               |> Ash.update()
    end

    test "does not find a record outside the parent()-scoped filter", %{
      project: project,
      foreign: foreign
    } do
      project
      |> Ash.Changeset.for_update(:assign, %{assignee_id: foreign.id})
      |> Ash.update()
      |> assert_filtered_out()
    end

    test "resolves parent() from the pending attributes on create", %{same_org: same_org} do
      assert {:ok, %{assignee_id: 1}} =
               Project
               |> Ash.Changeset.for_create(:create_assigned, %{
                 id: 2,
                 org_id: 10,
                 assignee_id: same_org.id
               })
               |> Ash.create()

      Project
      |> Ash.Changeset.for_create(:create_assigned, %{
        id: 3,
        org_id: 20,
        assignee_id: same_org.id
      })
      |> Ash.create()
      |> assert_filtered_out()
    end

    test "fails closed when parent() cannot be resolved from the source", %{
      project: project,
      same_org: same_org
    } do
      assert {:error, error} =
               project
               |> Ash.Changeset.for_update(:assign_reviewer, %{reviewer_id: same_org.id})
               |> Ash.update()

      assert Exception.message(error) =~ "parent(...)"
    end
  end

  describe "has_many lookup" do
    test "relates records that satisfy the parent()-scoped filter", %{
      project: project,
      same_org: same_org
    } do
      assert {:ok, %{employees: [%{id: 1}]}} =
               project
               |> Ash.Changeset.for_update(:add_employees, %{employee_ids: [same_org.id]})
               |> Ash.update(load: [:employees])
    end

    test "does not find records outside the parent()-scoped filter", %{
      project: project,
      foreign: foreign
    } do
      project
      |> Ash.Changeset.for_update(:add_employees, %{employee_ids: [foreign.id]})
      |> Ash.update()
      |> assert_filtered_out()
    end

    test "relates and creates records with `bulk?: true`", %{
      project: project,
      same_org: same_org
    } do
      assert {:ok, %{employees: employees}} =
               project
               |> Ash.Changeset.for_update(:bulk_add_employees, %{
                 employees: [%{id: same_org.id}, %{id: 3, org_id: 10, name: "new"}]
               })
               |> Ash.update()

      assert employees |> Enum.map(& &1.id) |> Enum.sort() == [1, 3]

      assert project
             |> Ash.load!(:employees)
             |> Map.get(:employees)
             |> Enum.map(& &1.id)
             |> Enum.sort() ==
               [1, 3]
    end
  end

  describe "many_to_many lookup" do
    test "relates records that satisfy the parent()-scoped filter", %{project: project} do
      Tag |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10}) |> Ash.create!()
      Tag |> Ash.Changeset.for_create(:create, %{id: 2, org_id: 20}) |> Ash.create!()

      assert {:ok, %{tags: [%{id: 1}]}} =
               project
               |> Ash.Changeset.for_update(:tag, %{tag_ids: [1]})
               |> Ash.update(load: [:tags])

      project
      |> Ash.Changeset.for_update(:tag, %{tag_ids: [2]})
      |> Ash.update()
      |> assert_filtered_out()
    end
  end

  describe "attribute multitenancy" do
    test "resolves parent() of the tenant attribute on create" do
      TenantEmployee |> Ash.Changeset.for_create(:create, %{id: 1}, tenant: 10) |> Ash.create!()
      TenantEmployee |> Ash.Changeset.for_create(:create, %{id: 2}, tenant: 20) |> Ash.create!()

      assert {:ok, %{assignee_id: 1, org_id: 10}} =
               TenantProject
               |> Ash.Changeset.for_create(:create_assigned, %{id: 1, assignee_id: 1}, tenant: 10)
               |> Ash.create()

      TenantProject
      |> Ash.Changeset.for_create(:create_assigned, %{id: 2, assignee_id: 2}, tenant: 10)
      |> Ash.create()
      |> assert_filtered_out()
    end

    test "resolves parent() through the join relationship against the pending join row" do
      TenantTag |> Ash.Changeset.for_create(:create, %{id: 1}, tenant: 10) |> Ash.create!()
      TenantTag |> Ash.Changeset.for_create(:create, %{id: 2}, tenant: 20) |> Ash.create!()

      project =
        TenantProject
        |> Ash.Changeset.for_create(:create, %{id: 1}, tenant: 10)
        |> Ash.create!()

      assert {:ok, %{tags: [%{id: 1}]}} =
               project
               |> Ash.Changeset.for_update(:tag, %{tag_ids: [1]}, tenant: 10)
               |> Ash.update(load: [:tags])

      project
      |> Ash.Changeset.for_update(:tag, %{tag_ids: [2]}, tenant: 10)
      |> Ash.update()
      |> assert_filtered_out()
    end
  end

  defp assert_filtered_out(result) do
    assert {:error, error} = result
    refute Exception.message(error) =~ "parent(...)"
  end
end
