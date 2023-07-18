defmodule Ash.Test.Flow.ChildResource do
  use Ash.Resource,
    authorizers: [Ash.Policy.Authorizer]

  actions do
    create :create do
      primary? true

      argument :parent_resource, Ash.Test.Flow.ParentResource

      change manage_relationship(:parent_resource, type: :append)
    end

    update :system_cancel do
      change set_attribute(:status, :canceled)

      reject :all
    end

    read :read do
      primary? true
      argument :build, :map

      prepare build(arg(:build))
    end
  end

  policies do
    policy action(:read) do
      authorize_if always()
    end

    policy action(:system_cancel) do
      authorize_if always()
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :status, :atom do
      default :active
      allow_nil? false
    end

    create_timestamp :created_at do
      private? false
    end

    update_timestamp :updated_at do
      private? false
    end
  end

  relationships do
    belongs_to :parent_resource, Ash.Test.Flow.ParentResource do
      allow_nil? false
    end
  end
end
