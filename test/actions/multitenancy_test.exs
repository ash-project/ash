defmodule Ash.Actions.MultitenancyTest do
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:org_id)
    end

    actions do
      create :create
      read :read
      update :update
      destroy :destroy
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :org_id, :uuid
    end

    relationships do
      has_many :posts, Ash.Actions.MultitenancyTest.Post, destination_field: :author_id
      has_many :comments, Ash.Actions.MultitenancyTest.Comment, destination_field: :commenter_id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :org_id, :uuid
    end

    actions do
      create :create
      read :read
      update :update
      destroy :destroy
    end

    relationships do
      has_many :comments, Ash.Actions.MultitenancyTest.Comment, destination_field: :post_id
      belongs_to :author, User
    end
  end

  defmodule Comment do
    @doc false

    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:context)
      global?(true)
    end

    actions do
      create :create
      read :read
      update :update
      destroy :destroy
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

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Comment)
      entry(Post)
      entry(User)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
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

      assert User |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record can be updated in a tenant", %{tenant1: tenant1, tenant2: tenant2} do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.update!()

      assert User |> Ash.Query.set_tenant(tenant2) |> Api.read!() == []
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      Comment
      |> Ash.Changeset.new()
      |> Ash.Changeset.set_tenant(tenant1)
      |> Api.create!()
      |> Ash.Changeset.new()
      |> Api.destroy!()
    end
  end
end
