defmodule Ash.Test.Flow.User do
  @moduledoc false
  use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

  resource do
    description "User model"
  end

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
    uuid_primary_key :id, description: "PK"
    attribute :first_name, :string, description: "User's first name"
    attribute :last_name, :string, description: "User's last name"
    attribute :email, :string, description: "User's email address"

    attribute :approved, :boolean do
      description "Is the user approved?"
      private? true
    end
  end

  relationships do
    belongs_to :org, Ash.Test.Flow.Org
  end
end
