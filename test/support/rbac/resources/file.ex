defmodule Ash.Policy.Test.Rbac.File do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  import Ash.Policy.Test.Rbac.Checks.RoleChecks, only: [can?: 1]

  policies do
    policy always() do
      authorize_if(can?(:file))
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
  end

  relationships do
    belongs_to(:organization, Ash.Policy.Test.Rbac.Organization)
  end
end
