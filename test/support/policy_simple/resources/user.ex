defmodule Ash.Test.Support.PolicySimple.User do
  @moduledoc false
  use Ash.Resource,
    api: Ash.Test.Support.PolicySimple.Api,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy action_type(:update) do
      authorize_if(expr(id == ^actor(:id)))
    end

    policy action_type(:read) do
      authorize_if(always())
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)
    attribute(:admin, :boolean)
    attribute(:manager, :boolean)
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  aggregates do
    max :most_recent_car, :cars, :inserted_at
  end

  calculations do
    calculate :restricted_from_driving,
              :boolean,
              expr(is_nil(most_recent_car) or most_recent_car >= ago(1, :microsecond))

    calculate :has_car,
              :boolean,
              expr(not is_nil(most_recent_car))
  end

  relationships do
    belongs_to(:organization, Ash.Test.Support.PolicySimple.Organization)
    has_many(:posts, Ash.Test.Support.PolicySimple.Post, destination_attribute: :author_id)

    many_to_many :cars, Ash.Test.Support.PolicySimple.Car do
      through(Ash.Test.Support.PolicySimple.CarUser)
      source_attribute_on_join_resource(:user_id)
      destination_attribute_on_join_resource(:car_id)
    end
  end
end
