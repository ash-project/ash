defmodule Ash.Policy.Test.Rbac.Organization do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    has_many :memberships, Ash.Policy.Test.Rbac.Membership do
      destination_field(:organization_id)
    end
  end
end
