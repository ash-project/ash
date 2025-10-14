# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

    policy action(:always_forbid) do
      access_type :strict
      forbid_if always()
    end

    policy action_type(:create) do
      access_type :runtime
      forbid_unless relating_to_actor(:author)
      authorize_if Ash.Test.Support.PolicyComplex.Comment.Checks.ManualCanSeePost
    end

    policy action(:read_with_runtime_check) do
      access_type :runtime
      authorize_if Ash.Test.Support.PolicyComplex.Comment.Checks.RuntimeCheck
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
    defaults [:read, :destroy, update: :*]

    read :always_forbid

    create :create do
      accept [:text]

      argument :post_id, :uuid do
        allow_nil? false
      end

      change manage_relationship(:post_id, :post, type: :append_and_remove)
      change relate_actor(:author)
    end

    read :read_through_post

    read :read_with_runtime_check
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
