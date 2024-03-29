defmodule Ash.Test.Support.PolicySimple.CarUser do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicySimple.Domain,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    belongs_to(:user, Ash.Test.Support.PolicySimple.User, public?: true)
    belongs_to(:car, Ash.Test.Support.PolicySimple.Car, public?: true)
  end
end
