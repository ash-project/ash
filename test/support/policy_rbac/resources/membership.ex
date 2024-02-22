defmodule Ash.Test.Support.PolicyRbac.Membership do
  @moduledoc false
  use Ash.Resource,
    api: Ash.Test.Support.PolicyRbac.Api,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)

    attribute :role, :atom do
      allow_nil?(false)
      constraints(one_of: [:admin, :member, :viewer])
    end

    attribute :resource, :atom do
      allow_nil?(false)
      constraints(one_of: [:file])
    end

    attribute :resource_id, :uuid do
      allow_nil?(false)
    end
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  relationships do
    belongs_to(:user, Ash.Test.Support.PolicyRbac.User)
    belongs_to(:organization, Ash.Test.Support.PolicyRbac.Organization)
  end
end
