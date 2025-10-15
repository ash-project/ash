# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicySimple.Car do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicySimple.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  defmodule Raising do
    @moduledoc false
    use Ash.Policy.SimpleCheck

    @impl true
    def describe(_), do: "should not be reachable"

    @impl true
    def match?(_, _, _) do
      raise "this should not be reachable"
    end
  end

  ets do
    private?(true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, update: :*]

    create :create do
      primary? true
      argument(:users, {:array, :uuid})
      change(manage_relationship(:users, type: :append_and_remove))
    end

    create :authorize_unless

    read :with_pagination do
      pagination offset?: true
    end
  end

  attributes do
    uuid_primary_key(:id)
    attribute :active, :boolean, default: true, public?: true
    timestamps()
  end

  policies do
    policy action(:authorize_unless) do
      authorize_if never()
      authorize_unless never()
      authorize_if never()
    end

    policy action_type([:update, :destroy]) do
      authorize_if expr(exists(users, id == ^actor(:id)))
    end

    policy_group action_type(:read) do
      policy do
        authorize_if expr(exists(users, id == ^actor(:id)))
      end

      policy [expr(active != true)] do
        forbid_if always()
      end
    end

    # this policy is testing the short circuiting evaluation of conditions
    policy [action_type(:read), never(), Raising] do
      authorize_if Raising
    end

    policy [action_type(:read), always()] do
      authorize_if always()
    end

    policy action_type([]) do
      authorize_if always()
    end
  end

  relationships do
    many_to_many :users, Ash.Test.Support.PolicySimple.User do
      public?(true)
      through(Ash.Test.Support.PolicySimple.CarUser)
      source_attribute_on_join_resource(:car_id)
      destination_attribute_on_join_resource(:user_id)
    end
  end
end
