defmodule Ash.Actions.MultitenancyTest do
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule User do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:org_id)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :org_id, :uuid
    end

    relationships do
      has_many :posts, Ash.Actions.MultitenancyTest.Post, destination_attribute: :author_id

      has_many :comments, Ash.Actions.MultitenancyTest.Comment,
        destination_attribute: :commenter_id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :org_id, :uuid
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      has_many :comments, Ash.Actions.MultitenancyTest.Comment, destination_attribute: :post_id
      belongs_to :author, User
    end
  end

  defmodule Comment do
    @doc false

    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:context)
      global?(false)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :org_id, :uuid
    end

    relationships do
      belongs_to :commenter, User
      belongs_to :post, Post
    end
  end

  describe "attribute multitenancy" do
    setup do
      %{tenant1: Ash.UUID.generate(), tenant2: Ash.UUID.generate()}
    end

    test "a simple write works when a tenant is specified", %{tenant1: tenant1} do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
    end

    test "a record written to one tenant cannot be read from another", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record written to one tenant cannot be read from another with aggregate queries", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Api.list!(:name) == []
    end

    test "a record can be updated in a tenant", %{tenant1: tenant1, tenant2: tenant2} do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.update!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      User
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.destroy!()
    end
  end

  describe "contextual multitenancy" do
    setup do
      %{tenant1: Ash.UUID.generate(), tenant2: Ash.UUID.generate()}
    end

    test "a simple write works when a tenant is specified", %{tenant1: tenant1} do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
    end

    test "a record written to one tenant cannot be read from another", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()

      assert Comment |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record can be updated in a tenant", %{tenant1: tenant1, tenant2: tenant2} do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.update!()

      assert Comment |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.destroy!()
    end

    test "a record cannot be read without tenant specified", %{
      tenant1: tenant1
    } do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()

      result = Comment |> Api.read()
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.TenantRequired{}]}} = result
    end

    test "an aggregate cannot be used without tenant specified", %{
      tenant1: tenant1
    } do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()

      result = User |> Api.count()
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.TenantRequired{}]}} = result
    end
  end
end
