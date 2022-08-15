defmodule Ash.Test.Support.PolicySimple.Car do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private?(true)
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true
      argument(:users, {:array, :uuid})
      change(manage_relationship(:users, type: :replace))
    end
  end

  attributes do
    uuid_primary_key(:id)
  end

  policies do
    policy always() do
      authorize_if(expr(users.id == ^actor(:id)))
    end
  end

  relationships do
    many_to_many :users, Ash.Test.Support.PolicySimple.User do
      through(Ash.Test.Support.PolicySimple.CarUser)
      source_attribute_on_join_resource(:car_id)
      destination_attribute_on_join_resource(:user_id)
    end
  end
end
