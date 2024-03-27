defmodule Ash.Actions.MultitenancyTest do
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:org_id)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    relationships do
      has_many :posts, Ash.Actions.MultitenancyTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_many :comments, Ash.Actions.MultitenancyTest.Comment,
        destination_attribute: :commenter_id,
        public?: true
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      has_many :comments, Ash.Actions.MultitenancyTest.Comment,
        destination_attribute: :post_id,
        public?: true

      belongs_to :author, User do
        public?(true)
      end
    end
  end

  defmodule Comment do
    @doc false

    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:context)
      global?(false)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    relationships do
      belongs_to :commenter, User do
        public?(true)
      end

      belongs_to :post, Post do
        public?(true)
      end
    end
  end

  describe "attribute multitenancy" do
    setup do
      %{tenant1: Ash.UUID.generate(), tenant2: Ash.UUID.generate()}
    end

    test "a simple write works when a tenant is specified", %{tenant1: tenant1} do
      User
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
    end

    test "a record written to one tenant cannot be read from another", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      User
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Ash.read!() == []
    end

    test "a record written to one tenant cannot be read from another with aggregate queries", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Ash.create!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Ash.list!(:name) == []
    end

    test "a record can be updated in a tenant", %{tenant1: tenant1, tenant2: tenant2} do
      User
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{}, tenant: tenant1)
      |> Ash.update!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Ash.read!() == []
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      User
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{}, tenant: tenant1)
      |> Ash.destroy!()
    end
  end

  describe "contextual multitenancy" do
    setup do
      %{tenant1: Ash.UUID.generate(), tenant2: Ash.UUID.generate()}
    end

    test "a simple write works when a tenant is specified", %{tenant1: tenant1} do
      Comment
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
    end

    test "a record written to one tenant cannot be read from another", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      Comment
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()

      assert Comment |> Ash.Query.set_tenant(tenant2) |> Ash.read!() == []
    end

    test "a record can be updated in a tenant", %{tenant1: tenant1, tenant2: tenant2} do
      Comment
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.Changeset.set_tenant(tenant1)
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{}, tenant: tenant1)
      |> Ash.update!()

      assert Comment |> Ash.Query.set_tenant(tenant2) |> Ash.read!() == []
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      Comment
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{}, tenant: tenant1)
      |> Ash.destroy!()
    end

    test "a record cannot be read without tenant specified", %{
      tenant1: tenant1
    } do
      Comment
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()

      result = Comment |> Ash.read()
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.TenantRequired{}]}} = result
    end

    test "an aggregate cannot be used without tenant specified", %{
      tenant1: tenant1
    } do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Ash.create!()

      result = User |> Ash.count()
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.TenantRequired{}]}} = result
    end
  end
end
