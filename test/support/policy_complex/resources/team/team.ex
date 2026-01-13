# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyComplex.Team do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  alias Ash.Test.Support.PolicyComplex.{Membership, User}

  policies do
    policy always() do
      access_type :strict

      authorize_if accessing_from(Membership, :team)
      authorize_if accessing_from(User, :teams)
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string, public?: true
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end

  relationships do
    has_many :memberships, Membership

    many_to_many :users, User do
      through Membership
    end
  end
end
