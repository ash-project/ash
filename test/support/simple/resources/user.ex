defmodule Ash.Policy.Test.Simple.User do
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
    belongs_to(:organization, Ash.Policy.Test.Simple.Organization)
    has_many(:posts, Ash.Policy.Test.Simple.Post, destination_field: :author_id)

    many_to_many :cars, Ash.Policy.Test.Simple.Car do
      through(Ash.Policy.Test.Simple.CarUser)
      source_field_on_join_table(:user_id)
      destination_field_on_join_table(:car_id)
    end
  end
end
