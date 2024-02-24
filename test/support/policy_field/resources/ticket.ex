defmodule Ash.Test.Support.PolicyField.Ticket do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicyField.Domain,
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
    attribute :internal_status, :string
    attribute :status, :string
    attribute :name, :string
  end

  relationships do
    belongs_to :representative, Ash.Test.Support.PolicyField.User do
      allow_nil? false
      attribute_writable? true
    end

    belongs_to :reporter, Ash.Test.Support.PolicyField.User do
      allow_nil? false
      attribute_writable? true
    end
  end

  policies do
    policy always() do
      authorize_if always()
    end
  end

  field_policies do
    field_policy :status do
      authorize_if relates_to_actor_via(:representative)
      authorize_if relates_to_actor_via(:reporter)
    end

    field_policy :internal_status, actor_attribute_equals(:role, :representative) do
      authorize_if always()
    end

    field_policy :internal_status, [
      accessing_from(Ash.Test.Support.PolicyField.User, :tickets),
      actor_attribute_equals(:role, :user)
    ] do
      authorize_if always()
    end

    field_policy [:name, :reporter_id, :representative_id] do
      authorize_if always()
    end
  end
end
