defmodule Ash.Test.Support.Flow.User do
  @moduledoc false
  use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

  actions do
    defaults [:read, :destroy]

    read :for_org do
      argument :org, :uuid, allow_nil?: false

      filter(expr(org_id == ^arg(:org)))
    end

    create :create do
      argument :org, :uuid, allow_nil?: false
      change manage_relationship(:org, type: :append_and_remove)
    end

    update :update do
      primary? true
    end

    update :approve do
      accept []
      change set_attribute(:approved, true)
    end

    update :unapprove do
      accept []
      change set_attribute(:approved, false)
    end
  end

  attributes do
    uuid_primary_key :id
    attribute :first_name, :string
    attribute :last_name, :string

    attribute :approved, :boolean do
      private? true
    end
  end

  relationships do
    belongs_to :org, Ash.Test.Support.Flow.Org
  end
end
