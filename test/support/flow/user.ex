defmodule Ash.Test.Flow.User do
  @moduledoc false
  use Ash.Resource, domain: Ash.Test.Flow.Domain, data_layer: Ash.DataLayer.Mnesia

  resource do
    description "User model"
  end

  actions do
    default_accept :*
    defaults [:read, :destroy]

    read :for_org do
      argument :org, :uuid, allow_nil?: false

      filter(expr(org_id == ^arg(:org)))
    end

    read :by_name do
      argument :name, :string, allow_nil?: false

      filter(expr(first_name == ^arg(:name)))
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

  code_interface do
    define :to_approved, action: :approve
  end

  attributes do
    uuid_primary_key :id, description: "PK"
    attribute :first_name, :string, description: "User's first name", public?: true
    attribute :last_name, :string, description: "User's last name", public?: true
    attribute :email, :string, description: "User's email address", public?: true

    attribute :approved, :boolean do
      description "Is the user approved?"
    end
  end

  relationships do
    belongs_to :org, Ash.Test.Flow.Org do
      public?(true)
    end
  end
end
