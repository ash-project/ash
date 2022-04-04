defmodule Ash.Test.Support.Flow do
  defmodule Org do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    identities do
      identity :unique_name, [:name]
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
      has_many :users, Ash.FlowTest.User
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:read, :update, :destroy]

      read :for_org do
        argument :org, :uuid, allow_nil?: false

        filter(expr(org_id == ^arg(:org)))
      end

      create :create do
        argument :org, :uuid, allow_nil?: false
        change manage_relationship(:org, type: :replace)
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
      belongs_to :org, Org
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(User)
      entry(Org)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end
end
