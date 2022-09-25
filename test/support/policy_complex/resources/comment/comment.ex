defmodule Ash.Test.Support.PolicyComplex.Comment do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    # you can't see comments on your post by people who aren't your friend
    # for testing purposes :)
    policy action_type(:read) do
      authorize_if expr(author == ^actor(:id))
      authorize_if expr(post.author == ^actor(:id))
      authorize_if expr(exists(author.friends, id == ^actor(:id)))
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
  end

  code_interface do
    define_for Ash.Test.Support.PolicyComplex.Api
    define :create, args: [:post_id, :text]
  end

  relationships do
    belongs_to(:author, Ash.Test.Support.PolicyComplex.User)
    belongs_to(:post, Ash.Test.Support.PolicyComplex.Post)
  end
end
