defmodule Ash.Test.Support.PolicyComplex.Post do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    bypass actor_attribute_equals(:super_user, true) do
      authorize_if always()
    end

    policy [action_type(:read)] do
      authorize_if relates_to_actor_via(:author)
      authorize_if relates_to_actor_via([:author, :friends])
    end

    policy action_type(:create) do
      authorize_if always()
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
    defaults [:destroy, update: :*]

    read :read do
      primary? true
    end

    create :create do
      accept [:text]
      change relate_actor(:author)
    end
  end

  aggregates do
    count :count_of_comments, :comments do
      public? true
    end

    count :count_of_commenters, [:comments, :author] do
      public? true
      uniq? true
    end
  end

  calculations do
    calculate :count_of_comments_calc, :integer, expr(count_of_comments) do
      public?(true)
    end
  end

  code_interface do
    define :create, args: [:text]
  end

  relationships do
    belongs_to :author, Ash.Test.Support.PolicyComplex.User do
      public?(true)
    end

    has_many :comments, Ash.Test.Support.PolicyComplex.Comment do
      public?(true)
    end
  end
end
