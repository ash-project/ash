defmodule Ash.Policy.Test.Simple.CarUser do
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
    belongs_to(:user, Ash.Policy.Test.Simple.User)
    belongs_to(:car, Ash.Policy.Test.Simple.Car)
  end
end
