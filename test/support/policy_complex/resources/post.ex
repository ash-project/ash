defmodule Ash.Test.Support.PolicyComplex.Post do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy action_type(:read) do
      authorize_if expr(author == ^actor(:id))
      authorize_if expr(exists(author.friends, id == ^actor(:id)))
    end

    policy action_type(:create) do
      authorize_if relating_to_actor(:author)
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
    defaults [:update, :destroy]

    read :read do
      primary? true
      pagination offset?: true
    end

    create :create do
      accept [:text]
      change relate_actor(:author)
    end
  end

  code_interface do
    define_for Ash.Test.Support.PolicyComplex.Api
    define :create, args: [:text]
  end

  relationships do
    belongs_to(:author, Ash.Test.Support.PolicyComplex.User)
    has_many :comments, Ash.Test.Support.PolicyComplex.Comment
  end
end
