defmodule Ash.Test.Support.PolicySimple.Trip do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  ets do
    private?(true)
  end

  policies do
    policy action_type(:read) do
      authorize_if(expr(car.users.id == ^actor(:id)))
    end

    policy action(:cant_load_car) do
      forbid_if loading(:car)
      authorize_if always()
    end
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    read :cant_load_car
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    belongs_to(:car, Ash.Test.Support.PolicySimple.Car)
  end
end
