defmodule Ash.Test.Support.PolicySimple.Context do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Support.PolicySimple.Domain,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [
      Ash.Policy.Authorizer
    ]

  policies do
    policy action_type(:create) do
      authorize_if always()
    end

    policy action_type(:read) do
      authorize_if(expr(^context(:name) == name))
    end
  end

  ets do
    private?(true)
  end

  attributes do
    uuid_primary_key(:id)
    attribute(:name, :string, public?: true)
  end

  actions do
    default_accept :*
    defaults [:read, :destroy, create: :*, update: :*]
  end
end
