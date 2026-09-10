# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ManageRelationshipParentFilterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Employee do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: [:id, :org_id, :name]]
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :org_id, :integer, allow_nil?: false, public?: true
      attribute :name, :string, allow_nil?: false, public?: true
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

      update :assign do
        require_atomic? false
        argument :assignee_id, :integer, allow_nil?: false
        change manage_relationship(:assignee_id, :assignee, type: :append_and_remove)
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
    end
  end

  test "a parent()-scoped relationship filter is forbidden in a managed lookup" do
    same_org =
      Employee
      |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10, name: "same org"})
      |> Ash.create!()

    _foreign =
      Employee
      |> Ash.Changeset.for_create(:create, %{id: 2, org_id: 20, name: "foreign org"})
      |> Ash.create!()

    project =
      Project
      |> Ash.Changeset.for_create(:create, %{id: 1, org_id: 10})
      |> Ash.create!()

    assert {:error, error} =
             project
             |> Ash.Changeset.for_update(:assign, %{assignee_id: same_org.id})
             |> Ash.update()

    assert Exception.message(error) =~ "parent(...)"

    assert {:error, _} =
             project
             |> Ash.Changeset.for_update(:assign, %{assignee_id: 2})
             |> Ash.update()
  end
end
