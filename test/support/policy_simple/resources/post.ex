defmodule Ash.Test.Support.PolicySimple.Post do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy action_type(:read) do
      description "You can read a post if you created it or if you own the organization"
      authorize_if(expr(author.id == ^actor(:id)))
      authorize_if(expr(organization.owner_id == ^actor(:id)))
    end

    policy action_type(:create) do
      description "Admins and managers can create posts"
      authorize_if(actor_attribute_equals(:admin, true))
      authorize_if(actor_attribute_equals(:manager, true))
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)

    attribute :text, :string do
      allow_nil?(false)
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true
      argument(:author, :uuid)
      change(manage_relationship(:author, type: :replace))

      argument(:organization, :uuid)
      change(manage_relationship(:organization, type: :replace))
    end
  end

  relationships do
    belongs_to(:organization, Ash.Test.Support.PolicySimple.Organization)
    belongs_to(:author, Ash.Test.Support.PolicySimple.User)
  end
end
