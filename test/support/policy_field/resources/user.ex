defmodule Ash.Test.Support.PolicyField.User do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key :id

    attribute :role, :atom do
      constraints one_of: [:user, :representative, :admin]
    end
  end

  relationships do
    has_many :tickets, Ash.Test.Support.PolicyField.Ticket do
      source_attribute :id
      destination_attribute :reporter_id
    end
  end

  aggregates do
    count :ticket_count, :tickets
  end

  policies do
    policy always() do
      authorize_if always()
    end
  end

  field_policies do
    field_policy_bypass :* do
      authorize_if actor_attribute_equals(:role, :admin)
    end

    field_policy :role do
      authorize_if actor_attribute_equals(:role, :representative)
    end

    field_policy :ticket_count do
      authorize_if actor_attribute_equals(:role, :reporter)
    end
  end
end
