defmodule Ash.Policy.Test.Simple.Organization do
  @moduledoc false
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets

  ets do
    private?(true)
  end

  actions do
    defaults [:read, :update, :destroy]

    create(:create) do
      primary? true
      argument(:owner, :uuid)
      change(manage_relationship(:owner, type: :replace))
    end
  end

  attributes do
    uuid_primary_key(:id)
  end

  relationships do
    has_many(:users, Ash.Policy.Test.Simple.User)
    has_many(:posts, Ash.Policy.Test.Simple.Post)
    belongs_to(:owner, Ash.Policy.Test.Simple.User)
  end
end
