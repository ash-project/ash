defmodule Ash.Test.Support.PolicyComplex.Bio do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
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
      public?(true)
      allow_nil?(false)
    end
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end

  code_interface do
    define :create, args: [:text]
  end

  relationships do
    belongs_to :user, Ash.Test.Support.PolicyComplex.User do
      public?(true)
    end
  end
end
