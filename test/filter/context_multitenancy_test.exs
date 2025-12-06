# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ContextMultitenancyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule TenantResource do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    defimpl Ash.ToTenant do
      def to_tenant(context, _resource), do: Ash.load!(context, :tenant, lazy?: true).tenant
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    calculations do
      calculate :tenant, :string, expr("tenant_" <> id)
    end

    actions do
      default_accept :*
      defaults [:read, update: :*, create: :*]
    end
  end

  defmodule MultiTenantRelated do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :context
    end

    attributes do
      attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update, :destroy]
    end

    relationships do
      has_one :tenant_resource, TenantResource do
        public? true
        no_attributes? true
        filter expr(tenant == ^tenant())
      end
    end
  end

  test "returns single resource for has_one" do
    tenant_resource =
      TenantResource
      |> Ash.Changeset.for_create(:create, %{id: 1000})
      |> Ash.create!()

    related =
      MultiTenantRelated
      |> Ash.Changeset.for_create(:create, %{id: 1001})
      |> Ash.create!(tenant: "tenant_1000", load: :tenant_resource)

    refute is_list(related.tenant_resource)
    assert tenant_resource.id == related.tenant_resource.id
  end
end
