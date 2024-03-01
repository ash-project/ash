defmodule Ash.Test.Support.PolicyComplex.Comment do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy action_type(:read) do
      authorize_if relates_to_actor_via(:author)
      # authorize_if relates_to_actor_via([:post, :author])
      authorize_if relates_to_actor_via([:author, :friends])
    end

    policy action_type(:create) do
      access_type :runtime
      forbid_unless relating_to_actor(:author)
      authorize_if Ash.Test.Support.PolicyComplex.Comment.Checks.ManualCanSeePost
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
    defaults [:read, :update, :destroy]

    create :create do
      accept [:text]

      argument :post_id, :uuid do
        allow_nil? false
      end

      change manage_relationship(:post_id, :post, type: :append_and_remove)
      change relate_actor(:author)
    end

    read :read_through_post
  end

  code_interface do
    define :create, args: [:post_id, :text]
  end

  relationships do
    belongs_to :author, Ash.Test.Support.PolicyComplex.User do
      public?(true)
    end

    belongs_to :post, Ash.Test.Support.PolicyComplex.Post do
      public?(true)
    end
  end
end
