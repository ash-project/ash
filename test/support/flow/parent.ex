defmodule Ash.Test.Flow.ParentResource do
  @moduledoc false
  use Ash.Resource,
    domain: Ash.Test.Flow.Domain,
    authorizers: [Ash.Policy.Authorizer],
    data_layer: Ash.DataLayer.Ets

  ets do
    private? true
  end

  actions do
    create :create do
      primary? true
    end

    read :read do
      primary? true

      argument :build, :map

      prepare build(arg(:build))
    end

    update :disable do
      argument :status, :atom do
        constraints one_of: [:canceled, :removed]
        default :canceled
      end

      change set_attribute(:status, :removed) do
        where [attribute_equals(:status, :canceled)]
      end

      accept [:status]

      manual Ash.Test.Actions.CancelParentResource
    end

    update :system_update

    update :system_disable do
      validate one_of(:status, [:canceled, :removed])

      change set_attribute(:status, :canceled) do
        where [absent([:status])]
      end

      accept [:status]
    end
  end

  code_interface do
    define :cancel, action: :disable
  end

  attributes do
    uuid_primary_key :id

    attribute :status, :atom do
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
    has_many :child_resources, Ash.Test.Flow.ChildResource
  end

  policies do
    policy action(:create) do
      authorize_if always()
    end

    policy action(:read) do
      authorize_if always()
    end

    policy action(:system_update) do
      authorize_if always()
    end

    policy action(:disable) do
      authorize_if always()
    end

    policy action(:system_disable) do
      authorize_if always()
    end
  end
end
