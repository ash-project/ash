defmodule Ash.Test.Support.PolicyRbac.User do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyRbac.Domain,
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
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  relationships do
    has_many(:memberships, Ash.Test.Support.PolicyRbac.Membership,
      destination_attribute: :user_id,
      public?: true
    )

    belongs_to(:organization, Ash.Test.Support.PolicyRbac.Organization, public?: true)
  end
end
