defmodule Ash.Test.Support.PolicyComplex.Bio do
  @moduledoc false
  use Ash.Resource,
    api: Ash.Test.Support.PolicyComplex.Api,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy always() do
      authorize_if accessing_from(Ash.Test.Support.PolicyComplex.User, :bio)
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)

    attribute :text, :string do
      allow_nil?(false)
    end
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  code_interface do
    define :create, args: [:text]
  end

  relationships do
    belongs_to :user, Ash.Test.Support.PolicyComplex.User
  end
end
