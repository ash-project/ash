defmodule Ash.Test.Support.PolicyRbac.Organization do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyRbac.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  actions do
    default_accept :*
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    has_many :memberships, Ash.Test.Support.PolicyRbac.Membership do
      public?(true)
      destination_attribute(:organization_id)
    end

    has_many :files, Ash.Test.Support.PolicyRbac.File do
      public?(true)
    end
  end
end
