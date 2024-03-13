defmodule Ash.Test.Support.PolicyComplex.User do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    # For testing we need to be able to read/create/update users
    policy action_type(:read) do
      forbid_if selecting(:private_email)
      authorize_if always()
    end

    policy action_type(:create) do
      authorize_if always()
    end

    policy action_type(:update) do
      authorize_if always()
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)

    attribute :name, :string do
      public?(true)
      allow_nil? false
    end

    attribute :private_email, :string do
      public?(true)
    end
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, update: :*]

    create :create do
      primary? true
      argument :email, :string
      argument :bio_text, :string

      change manage_relationship(:bio_text, :bio, type: :create, value_is_key: :text)
      change set_attribute(:private_email, arg(:email))
    end

    update :add_friend do
      accept []

      argument :friend_id, :uuid do
        allow_nil? false
      end

      change Ash.Test.Support.PolicyComplex.User.Changes.AddFriend
    end

    update :set_bio do
      argument :bio, :string do
        allow_nil? false
      end

      change manage_relationship(:bio, type: :direct_control, value_is_key: :text)
    end
  end

  code_interface do
    define :create, args: [:name]
    define :add_friend, args: [:friend_id]
    define :set_bio, args: [:bio]
  end

  aggregates do
    first :bio_text, :bio, :text do
      public? true
    end
  end

  relationships do
    has_many(:posts, Ash.Test.Support.PolicyComplex.Post,
      destination_attribute: :author_id,
      public?: true
    )

    has_many :friends, Ash.Test.Support.PolicyComplex.User do
      public?(true)
      manual Ash.Test.Support.PolicyComplex.User.Relationships.Friends
    end

    has_one :best_friend, Ash.Test.Support.PolicyComplex.User do
      public?(true)
      manual Ash.Test.Support.PolicyComplex.User.Relationships.BestFriend
    end

    has_one :bio, Ash.Test.Support.PolicyComplex.Bio do
      public?(true)
    end
  end
end
