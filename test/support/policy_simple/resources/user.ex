defmodule Ash.Test.Support.PolicySimple.User do
  @moduledoc false
  use Ash.Resource,
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

  relationships do
    belongs_to(:organization, Ash.Test.Support.PolicySimple.Organization)
    has_many(:posts, Ash.Test.Support.PolicySimple.Post, destination_field: :author_id)

    many_to_many :cars, Ash.Test.Support.PolicySimple.Car do
      through(Ash.Test.Support.PolicySimple.CarUser)
      source_field_on_join_table(:user_id)
      destination_field_on_join_table(:car_id)
    end
  end
end
