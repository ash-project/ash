defmodule Ash.Test.Support.PolicySimple.Tweet do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private?(true)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key(:id)
  end

  policies do
    bypass expr(^actor(:admin)) do
      authorize_if always()
    end

    policy always() do
      authorize_if(expr(user_id == ^actor(:id)))
    end
  end

  relationships do
    belongs_to :user, Ash.Test.Support.PolicySimple.User
  end
end
