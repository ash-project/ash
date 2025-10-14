# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.MultitenancyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule MultiTenant do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :attribute
      attribute :owner
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, update: :*]

      create :create do
        argument :multitenant_related, {:array, :map}

        change manage_relationship(:multitenant_related, type: :direct_control)
      end

      destroy :destroy do
        primary? true
        change cascade_destroy(:multitenant_related)
      end
    end

    relationships do
      has_many :multitenant_related, Ash.Test.MultitenancyTest.MultiTenantRelated
    end
  end

  defmodule MultiTenantRelated do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :attribute
      attribute :owner
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner, :integer, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    relationships do
      belongs_to :multi_tenant, MultiTenant, public?: true, attribute_type: :integer
    end
  end

  test "reading an object doesn't require multitenancy attribute in the primary key" do
    MultiTenant
    |> Ash.Changeset.for_create(:create, %{id: 1000, owner: 1})
    |> Ash.create!(tenant: 1)

    MultiTenant
    |> Ash.get!(1000, tenant: 1)
  end

  test "cascade destroy works" do
    MultiTenant
    |> Ash.Changeset.for_create(:create, %{
      id: 1000,
      owner: 1,
      multitenant_related: [%{owner: 1, id: 1}, %{owner: 1, id: 2}, %{owner: 1, id: 3}]
    })
    |> Ash.create!(tenant: 1)

    MultiTenant
    |> Ash.get!(1000, tenant: 1)
    |> Ash.destroy!(tenant: 1)
  end

  test "should preserve loaded relationships when performing no-op update" do
    multi_tenant =
      MultiTenant
      |> Ash.Changeset.for_create(:create, %{id: 1000, owner: 1})
      |> Ash.create!(load: [:multitenant_related], tenant: 1)

    assert [] = multi_tenant.multitenant_related

    multi_tenant =
      multi_tenant
      |> Ash.Changeset.for_update(:update, %{})
      |> Ash.update!(tenant: 1)

    assert [] = multi_tenant.multitenant_related
  end

  defmodule NonMultiTenant do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
      attribute :owner, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end
  end

  test "reading an object without multitenancy requires attribute in the primary key" do
    NonMultiTenant
    |> Ash.Changeset.for_create(:create, %{id: 1000, owner: 1})
    |> Ash.create!()

    ExUnit.Assertions.assert_raise(Ash.Error.Invalid, fn ->
      NonMultiTenant
      |> Ash.get!(1000)
    end)

    NonMultiTenant
    |> Ash.get!(%{id: 1000, owner: 1})
  end
end
