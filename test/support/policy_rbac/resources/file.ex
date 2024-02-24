defmodule Ash.Test.Support.PolicyRbac.File do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyRbac.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  import Ash.Test.Support.PolicyRbac.Checks.RoleChecks, only: [can?: 1]

  policies do
    policy always() do
      # anyone can create files
      authorize_if(action_type(:create))
      authorize_if(can?(:file))
    end

    policy actor_attribute_equals(:rel_check, true) do
      forbid_if selecting(:forbidden)
      authorize_if always()
    end
  end

  ets do
    private?(true)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key(:id)
    attribute(:name, :string)
    attribute(:forbidden, :string)
  end

  relationships do
    belongs_to(:organization, Ash.Test.Support.PolicyRbac.Organization)
  end
end
