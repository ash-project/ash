defmodule Ash.Test.Support.PolicyComplex.FriendLink do
  @moduledoc false
  use Ash.Resource,
    api: Ash.Test.Support.PolicyComplex.Api,
    data_layer: Ash.DataLayer.Ets,
    authorizers: [Ash.Policy.Authorizer]

  ets do
    private? true
  end

  policies do
    policy action_type(:create) do
      authorize_if relating_to_actor(:source)
      authorize_if relating_to_actor(:destination)
    end

    # note to reader: in real life you'd want some kind of invite/approval system
    policy action_type(:read) do
      authorize_if relates_to_actor_via(:source)
      authorize_if relates_to_actor_via(:destination)
    end
  end

  actions do
    read :read do
      primary? true
    end

    create :create do
      change Ash.Test.Support.PolicyComplex.FriendLink.Changes.SortIds
    end
  end

  relationships do
    belongs_to :source, Ash.Test.Support.PolicyComplex.User do
      allow_nil? false
      primary_key? true
      attribute_writable? true
    end

    belongs_to :destination, Ash.Test.Support.PolicyComplex.User do
      allow_nil? false
      primary_key? true
      attribute_writable? true
    end
  end
end
