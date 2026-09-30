# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.MultitenancyAncestorAttributesTest do
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule DepartmentTenant do
    @moduledoc false
    defstruct [:id, :organization_id]

    defimpl Ash.ToTenant do
      def to_tenant(department, _resource), do: department.id
    end

    defimpl Ash.ToAncestorTenants do
      def to_ancestor_tenants(department, _resource), do: [department.organization_id]
    end
  end

  defmodule TeamTenant do
    @moduledoc false
    defstruct [:id, :organization_id, :department_id]

    defimpl Ash.ToTenant do
      def to_tenant(team, _resource), do: team.id
    end

    defimpl Ash.ToAncestorTenants do
      def to_ancestor_tenants(team, _resource), do: [team.organization_id, team.department_id]
    end
  end

  defmodule Customer do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:department_id)
      ancestor_attributes([:organization_id])
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :organization_id, :uuid do
        public?(true)
      end

      attribute :department_id, :uuid do
        public?(true)
      end
    end
  end

  defmodule Task do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:team_id)
      ancestor_attributes([:organization_id, :department_id])
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :organization_id, :uuid do
        public?(true)
      end

      attribute :department_id, :uuid do
        public?(true)
      end

      attribute :team_id, :uuid do
        public?(true)
      end
    end
  end

  setup do
    organization_id = Ash.UUID.generate()
    other_organization_id = Ash.UUID.generate()
    department_a = %DepartmentTenant{id: Ash.UUID.generate(), organization_id: organization_id}

    %{
      organization_id: organization_id,
      other_organization_id: other_organization_id,
      department_a: department_a
    }
  end

  describe "ancestor derivation" do
    test "a tenant that can't derive its ancestors raises instead of silently dropping the ancestor filters" do
      # A bare id has no Ash.ToAncestorTenants implementation, so without the
      # raise this read would succeed with only the department filter applied.
      bare_department_id = Ash.UUID.generate()

      assert_raise Ash.Error.Unknown, ~r/Ash.ToAncestorTenants/, fn ->
        Ash.read!(Customer, tenant: bare_department_id)
      end
    end
  end

  describe "create" do
    test "create stamps derived ancestor attributes, not just the tenant attribute", %{
      organization_id: organization_id,
      department_a: department_a
    } do
      customer =
        Customer
        |> Ash.Changeset.for_create(:create, %{name: "customer"}, tenant: department_a)
        |> Ash.create!()

      assert customer.department_id == department_a.id
      assert customer.organization_id == organization_id

      team = %TeamTenant{
        id: Ash.UUID.generate(),
        organization_id: organization_id,
        department_id: department_a.id
      }

      task =
        Task
        |> Ash.Changeset.for_create(:create, %{name: "task"}, tenant: team)
        |> Ash.create!()

      assert task.team_id == team.id
      assert task.department_id == department_a.id
      assert task.organization_id == organization_id
    end
  end

  describe "read" do
    test "a tenant with the same department id but another organization reads nothing", %{
      other_organization_id: other_organization_id,
      department_a: department_a
    } do
      customer =
        Customer
        |> Ash.Changeset.for_create(:create, %{name: "customer"}, tenant: department_a)
        |> Ash.create!()

      assert [%{id: read_id}] = Customer |> Ash.read!(tenant: department_a)
      assert read_id == customer.id

      # The department filter alone matches this row; only the ancestor
      # filter can exclude it
      wrong_organization_department = %DepartmentTenant{
        id: department_a.id,
        organization_id: other_organization_id
      }

      assert [] = Customer |> Ash.read!(tenant: wrong_organization_department)
    end
  end

  describe "update" do
    test "update can't reach a row whose ancestors don't match the tenant", %{
      other_organization_id: other_organization_id,
      department_a: department_a
    } do
      customer =
        Customer
        |> Ash.Changeset.for_create(:create, %{name: "before"}, tenant: department_a)
        |> Ash.create!()

      wrong_organization_department = %DepartmentTenant{
        id: department_a.id,
        organization_id: other_organization_id
      }

      assert {:error, _} =
               customer
               |> Ash.Changeset.for_update(:update, %{name: "after"},
                 tenant: wrong_organization_department
               )
               |> Ash.update()

      assert [%{name: "before"}] = Customer |> Ash.read!(tenant: department_a)
    end
  end
end
