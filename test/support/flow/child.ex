defmodule Ash.Test.Flow.ChildResource do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Flow.Domain,
    authorizers: [Ash.Policy.Authorizer],
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  actions do
    default_accept :*

    create :create do
      primary? true

      argument :parent_resource, :map

      change manage_relationship(:parent_resource, type: :append)
    end

    update :system_cancel do
      accept []
      change set_attribute(:status, :canceled)
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
      public?(true)
      default :active
      allow_nil? false
    end

    create_timestamp :created_at do
      public?(true)
    end

    update_timestamp :updated_at do
      public?(true)
    end
  end

  relationships do
    belongs_to :parent_resource, Ash.Test.Flow.ParentResource do
      public?(true)
      allow_nil? false
    end
  end
end
