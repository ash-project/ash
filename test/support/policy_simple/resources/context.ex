# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicySimple.Context do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicySimple.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  alias Ash.Test.Support.PolicySimple.Context.Changes.DoChange

  policies do
    policy action_type(:create) do
      authorize_if always()
    end

    policy action_type([:read, :update]) do
      authorize_if(expr(^context(:name) == name))
      authorize_if relates_to_actor_via(:user)
    end
  end

  ets do
    private?(true)
  end

  relationships do
    belongs_to(:user, Ash.Test.Support.PolicySimple.User, public?: true)
  end

  attributes do
    uuid_primary_key(:id)
    attribute(:name, :string, public?: true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*]

    update :update do
      require_atomic? false
      argument :name, :string
      change DoChange
    end
  end
end
