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

    create :create_foo do
      argument :foo, :string
    end
  end

  attributes do
    uuid_primary_key(:id)
  end

  policies do
    bypass expr(^actor(:admin)) do
      authorize_if always()
    end

    policy action_type([:read, :update, :destroy]) do
      authorize_if(expr(user_id == ^actor(:id)))
    end

    policy action(:create) do
      authorize_if relating_to_actor(:user)
    end

    policy action(:create_foo) do
      authorize_if expr(^arg(:foo) == "foo")
    end
  end

  relationships do
    belongs_to :user, Ash.Test.Support.PolicySimple.User do
      attribute_writable? true
    end
  end
end
