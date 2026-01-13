# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyComplex.Membership do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyComplex.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  alias Ash.Test.Support.PolicyComplex.{Team, User}

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key :id
  end

  policies do
    policy always() do
      access_type :strict

      authorize_if accessing_from(Team, :membership)
      authorize_if accessing_from(User, :membership)
      authorize_if accessing_from(User, :teams_join_assoc)
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]
  end

  relationships do
    belongs_to :team, Team
    belongs_to :user, User
  end
end
