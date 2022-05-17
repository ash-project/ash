defmodule Ash.Policy.Test.Rbac.User do
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
    has_many(:memberships, Ash.Policy.Test.Rbac.Membership, destination_field: :user_id)

    belongs_to(:organization, Ash.Policy.Test.Rbac.Organization)
  end
end
