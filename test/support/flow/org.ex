defmodule Ash.Test.Flow.Org do
  @moduledoc false
  use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

  resource do
    description "Org model"
  end

  identities do
    identity :unique_name, [:name], pre_check_with: Ash.Test.Flow.Api
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    read :by_name do
      argument :name, :string, allow_nil?: false
      get? true

      filter expr(name == ^arg(:name))
    end
  end

  attributes do
    uuid_primary_key :id
    attribute :name, :string
  end

  relationships do
    has_many :users, Ash.Test.Flow.User
  end
end
