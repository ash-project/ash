defmodule Ash.Policy.Test.Simple.Trip do
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
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    belongs_to(:car, Ash.Policy.Test.Simple.Car)
  end
end
