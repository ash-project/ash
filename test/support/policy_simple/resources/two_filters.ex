# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicySimple.TwoFilters do
  @moduledoc false

  use Ash.Resource,
    domain: Ash.Test.Support.PolicySimple.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private?(true)
  end

  actions do
    defaults [:read, create: [:user_id]]
  end

  attributes do
    uuid_primary_key :id
  end

  relationships do
    belongs_to :user, Ash.Test.Support.PolicySimple.User
    belongs_to :user2, Ash.Test.Support.PolicySimple.User
  end

  policies do
    policy relates_to_actor_via(:user) do
      authorize_if always()
    end

    policy relates_to_actor_via(:user2) do
      authorize_if always()
    end
  end
end
