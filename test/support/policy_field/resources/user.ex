# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Support.PolicyField.User do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyField.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]

    create :special_create
  end

  attributes do
    uuid_primary_key :id

    attribute :role, :atom do
      public?(true)
      constraints one_of: [:user, :representative, :admin]
    end

    attribute :points, :integer do
      public?(true)
      # only you can see your own points
    end

    attribute :see_if_just_created, :string do
      public? true
      default "was just created with special create"
    end

    attribute :top_secret, :string do
      public?(false)
    end
  end

  relationships do
    has_many :tickets, Ash.Test.Support.PolicyField.Ticket do
      public?(true)
      source_attribute :id
      destination_attribute :reporter_id
    end
  end

  aggregates do
    count :ticket_count, :tickets do
      public? true
    end
  end

  policies do
    policy always() do
      authorize_if always()
    end
  end

  field_policies do
    private_fields :show

    field_policy_bypass :* do
      authorize_if actor_attribute_equals(:role, :admin)
    end

    field_policy :see_if_just_created do
      authorize_if just_created_with_action(:special_create)
    end

    field_policy :role do
      authorize_if actor_attribute_equals(:role, :representative)
    end

    field_policy :points do
      authorize_if expr(id == ^actor(:id))
    end

    field_policy :ticket_count, [
      actor_attribute_equals(:role, :representative),
      accessing_from(Ash.Test.Support.PolicyField.Ticket, :reporter)
    ] do
      authorize_if always()
    end

    field_policy :ticket_count, actor_attribute_equals(:role, :reporter) do
      authorize_if always()
    end
  end
end
