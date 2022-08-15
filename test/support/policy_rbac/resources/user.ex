defmodule Ash.Test.Support.PolicyRbac.User do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  relationships do
    has_many(:memberships, Ash.Test.Support.PolicyRbac.Membership, destination_attribute: :user_id)

    belongs_to(:organization, Ash.Test.Support.PolicyRbac.Organization)
  end
end
