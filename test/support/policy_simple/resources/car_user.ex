defmodule Ash.Test.Support.PolicySimple.CarUser do
  @moduledoc false
  use Ash.Resource,
    api: Ash.Test.Support.PolicySimple.Api,
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
    belongs_to(:user, Ash.Test.Support.PolicySimple.User)
    belongs_to(:car, Ash.Test.Support.PolicySimple.Car)
  end
end
