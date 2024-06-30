defmodule Ash.Actions.MultitenancyTest do
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:org_id)
    end

    policies do
      policy action(:has_policies) do
        authorize_if relates_to_actor_via(:self)
      end

      policy always() do
        authorize_if always()
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :has_policies

      read :allow_global do
        multitenancy(:allow_global)
      end

      read :bypass_tenant do
        multitenancy(:bypass)
      end
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

      has_one :self, __MODULE__, destination_attribute: :id, source_attribute: :id
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

      read :allow_global do
        multitenancy(:allow_global)
      end

      read :bypass_tenant do
        multitenancy(:bypass)
      end
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

  defmodule Tenant do
    @doc false

    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      integer_primary_key :id, writable?: true
    end

    defimpl Ash.ToTenant do
      def to_tenant(tenant, _resource), do: tenant.id
    end
  end

  defmodule OtherThingName do
    use Ash.Resource.Calculation

    def load(_, _, _) do
      [other_thing: [:name]]
    end

    def calculate(records, _, _) do
      Enum.map(records, &(&1.other_thing && &1.other_thing.name))
    end
  end

  defmodule OtherThingNameReversed do
    use Ash.Resource.Calculation

    def calculate(records, _, _) do
      # Normally you would just load :other_thing in the load callback
      # This simulates cases where you're conditionally loading based on
      # runtime conditions so you do it manually in the calculation
      records
      |> Ash.load!(:other_thing_name)
      |> Enum.map(&(&1.other_thing_name && String.reverse(&1.other_thing_name)))
    end
  end

  defmodule MultitenantThing do
    @doc false

    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    multitenancy do
      strategy(:attribute)
      attribute(:tenant_id)
      parse_attribute {MultitenantThing, :parse_tenant, []}
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :tenant_id, :string do
        public?(true)
      end

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :other_thing, MultitenantThing, public?: true
    end

    calculations do
      calculate :other_thing_name, :string, OtherThingName, public?: true
      calculate :other_thing_name_reversed, :string, OtherThingNameReversed, public?: true
    end

    def parse_tenant(id), do: "tenant_#{id}"
  end

  describe "ToTenant and parse_attribute are applied in the correct order" do
    setup do
      tenant1 =
        Tenant
        |> Ash.Changeset.for_create(:create, %{id: 1})
        |> Ash.create!()

      tenant2 =
        Tenant
        |> Ash.Changeset.for_create(:create, %{id: 2})
        |> Ash.create!()

      %{tenant1: tenant1, tenant2: tenant2}
    end

    test "with tenant on changeset and query", %{tenant1: tenant1, tenant2: tenant2} do
      thing =
        MultitenantThing
        |> Ash.Changeset.for_create(:create, %{name: "foo"}, tenant: tenant1)
        |> Ash.create!()

      thing
      |> Ash.Changeset.for_update(:update, %{name: "bar"}, tenant: tenant1)
      |> Ash.update!()

      assert [%{tenant_id: "tenant_1", name: "bar"}] =
               MultitenantThing
               |> Ash.Query.set_tenant(tenant1)
               |> Ash.read!()

      assert MultitenantThing |> Ash.Query.set_tenant(tenant2) |> Ash.read!() == []
    end

    test "with tenant in options", %{tenant1: tenant1, tenant2: tenant2} do
      thing =
        MultitenantThing
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!(tenant: tenant1)

      thing
      |> Ash.Changeset.for_update(:update, %{name: "bar"})
      |> Ash.update!(tenant: tenant1)

      assert [%{tenant_id: "tenant_1", name: "bar"}] =
               MultitenantThing
               |> Ash.read!(tenant: tenant1)

      assert MultitenantThing |> Ash.read!(tenant: tenant2) == []
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

    test "prior filters are not affected by the addition of a multitenancy attribute", %{
      tenant1: tenant1
    } do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()

      # assert [fetched_user1, fetched_user2] =
      User
      |> Ash.Query.for_read(:has_policies, %{}, actor: user1, tenant: tenant1)
      |> Ash.read!()
    end

    test "supports :allow_global multitenancy on the read action", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()

      user2 =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant2)
        |> Ash.create!()

      assert [fetched_user1, fetched_user2] =
               User
               |> Ash.Query.for_read(:allow_global)
               |> Ash.read!()

      assert Enum.sort([fetched_user1.id, fetched_user2.id]) == Enum.sort([user1.id, user2.id])

      assert [fetched_user1] =
               User
               |> Ash.Query.for_read(:allow_global)
               |> Ash.Query.set_tenant(tenant1)
               |> Ash.read!()

      assert fetched_user1.id == user1.id
    end

    test "supports :bypass multitenancy on the read action", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()

      user2 =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant2)
        |> Ash.create!()

      assert [fetched_user1, fetched_user2] =
               User
               |> Ash.Query.for_read(:bypass_tenant)
               |> Ash.Query.set_tenant(tenant1)
               |> Ash.read!()

      assert Enum.sort([fetched_user1.id, fetched_user2.id]) == Enum.sort([user1.id, user2.id])
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

    test "a record for a different tenant cant be updated from the other one", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      assert_raise Ash.Error.Invalid, ~r/Attempted to update stale record/, fn ->
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()
        |> Ash.Changeset.for_update(:update, %{name: "new name"}, tenant: tenant2)
        |> Ash.update!()
      end
    end

    test "a record can be destroyed in a tenant", %{tenant1: tenant1} do
      User
      |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
      |> Ash.create!()
      |> Ash.Changeset.for_update(:update, %{}, tenant: tenant1)
      |> Ash.destroy!()
    end

    test "tenant is set on data in calculations", %{tenant1: tenant1} do
      thing1 =
        MultitenantThing
        |> Ash.Changeset.for_create(:create, %{name: "foo"}, tenant: tenant1)
        |> Ash.create!()

      thing2 =
        MultitenantThing
        |> Ash.Changeset.for_create(:create, %{name: "bar", other_thing_id: thing1.id},
          tenant: tenant1
        )
        |> Ash.create!()

      thing2 =
        thing2
        |> Ash.Changeset.for_update(:update, %{name: "bar updated"})
        |> Ash.update!()

      %{other_thing_name_reversed: "oof"} = Ash.load!(thing2, :other_thing_name_reversed)

      %{other_thing_name_reversed: "oof"} =
        MultitenantThing
        |> Ash.get!(thing2.id, tenant: tenant1, load: :other_thing_name_reversed)
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

    test "supports :allow_global multitenancy on the read action", %{
      tenant1: tenant1,
      tenant2: tenant2
    } do
      comment1 =
        Comment
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()

      _comment2 =
        Comment
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant2)
        |> Ash.create!()

      # We can't actually read all the posts because the ETS data layer
      # can't query across contextual tenants, but the read action
      # doesn't raise Ash.Error.Invalid.TenantRequired
      Comment
      |> Ash.Query.for_read(:allow_global)
      |> Ash.read!()

      assert [fetched_comment1] =
               Comment
               |> Ash.Query.for_read(:allow_global)
               |> Ash.Query.set_tenant(tenant1)
               |> Ash.read!()

      assert fetched_comment1.id == comment1.id
    end

    test "supports :bypass multitenancy on the read action" do
      # We can't actually read all the posts because the ETS data layer
      # can't query across contextual tenants, but the read action
      # doesn't raise Ash.Error.Invalid.TenantRequired
      Comment
      |> Ash.Query.for_read(:bypass_tenant)
      |> Ash.read!()
    end
  end
end
