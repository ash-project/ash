# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
      authorize_if action(:erasable)
      authorize_if relates_to_actor_via(:author)
      authorize_if relates_to_actor_via([:author, :friends])
    end

    policy action_type(:create) do
      authorize_if always()
    end

    policy action([:erase, :erasable]) do
      authorize_if expr(has_context)
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
      primary? true
      accept [:text]
      change relate_actor(:author)
    end

    read :erasable

    update :erase do
      change set_attribute(:text, "[deleted]")
    end
  end

  aggregates do
    count :count_of_comments, :comments do
      public? true
    end

    count :always_forbidden_comments, :comments do
      public? true
      read_action :always_forbid
    end

    count :always_forbidden_author, :author do
      public? true
      read_action :always_forbid
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

    calculate :has_context, :boolean, expr(id == ^context(:post_id) and author_id == ^actor(:id))
  end

  code_interface do
    define :create, args: [:text]
    define :erase
    define :erasable
  end

  relationships do
    belongs_to :author, Ash.Test.Support.PolicyComplex.User do
      public?(true)
    end

    belongs_to :forbidden_field_author, Ash.Test.Support.PolicyComplex.User do
      source_attribute :author_id
      define_attribute? false
      authorize_read_with(:error)
      allow_forbidden_field?(true)
    end

    has_many :comments, Ash.Test.Support.PolicyComplex.Comment do
      public?(true)
    end
  end
end
