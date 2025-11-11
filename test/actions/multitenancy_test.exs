# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.MultitenancyTest do
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule PassingIfTenantIsNotNil do
    use Ash.Policy.SimpleCheck

    def describe(_), do: "pass if tenant is present"

    def match?(_actor, %{query: %{tenant: tenant}}, _) when not is_nil(tenant) do
      true
    end
  end

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
      policy action(:bypass_tenant_with_policy) do
        authorize_if PassingIfTenantIsNotNil
      end

      policy action(:bypass_all_tenant_with_policy) do
        authorize_if PassingIfTenantIsNotNil
      end

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

      read :bypass_tenant_with_policy do
        multitenancy(:bypass)
      end

      read :bypass_all_tenant_with_policy do
        multitenancy(:bypass_all)
      end

      read :bypass_all do
        multitenancy(:bypass_all)
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

  defmodule Like do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy(:attribute)
      attribute(:org_id)
    end

    actions do
      default_accept :*
      defaults [:read, :create]
    end

    attributes do
      uuid_primary_key :id

      attribute :org_id, :uuid do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Post do
        public?(true)
      end
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

      has_many :likes, Ash.Actions.MultitenancyTest.Like,
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

    test "an error is produced when a tenant is not specified" do
      assert_raise Ash.Error.Invalid, ~r/require a tenant to be specified/, fn ->
        User
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()
      end
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

      User
      |> Ash.Query.for_read(:has_policies, %{}, actor: user1, tenant: tenant1)
      |> Ash.read!()
    end

    test ":bypass and :bypass_all does not alter the initial query", %{tenant1: tenant1} do
      User
      |> Ash.Query.for_read(:bypass_tenant_with_policy, %{}, tenant: tenant1)
      |> Ash.read!()

      User
      |> Ash.Query.for_read(:bypass_all_tenant_with_policy, %{}, tenant: tenant1)
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

    test "updates require a tenant as well", %{tenant1: tenant1} do
      assert_raise Ash.Error.Invalid, ~r/require a tenant to be specified/, fn ->
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()
        |> Map.update!(:__metadata__, &Map.delete(&1, :tenant))
        |> Ash.Changeset.for_update(:update, %{})
        |> Ash.update!()
      end
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

    test "supports :bypass_all multitenancy on a read action", %{tenant1: tenant1} do
      user =
        User
        |> Ash.Changeset.for_create(:create, %{}, tenant: tenant1)
        |> Ash.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{author_id: user.id}, tenant: tenant1)
        |> Ash.create!()

      like =
        Like
        |> Ash.Changeset.for_create(:create, %{post_id: post.id}, tenant: tenant1)
        |> Ash.create!()

      [%{posts: [%{likes: [%{id: like_id}]}]}] =
        User
        |> Ash.Query.for_read(:bypass_all, %{})
        |> Ash.Query.load(posts: :likes)
        |> Ash.read!()

      assert like_id == like.id
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

    test "an aggregate can be used with a tenant specified", %{
      tenant1: tenant1
    } do
      assert 0 = Ash.count!(Comment, tenant: tenant1)
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

  describe "tenant_from_attribute option" do
    defmodule TenantFromAttributeThing do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      ets do
        private?(true)
      end

      multitenancy do
        strategy(:attribute)
        attribute(:tenant_id)
        parse_attribute {__MODULE__, :parse_tenant, []}
        tenant_from_attribute({__MODULE__, :format_tenant, []})
      end

      actions do
        default_accept :*
        defaults [:read, :destroy, create: :*, update: :*]
      end

      attributes do
        uuid_primary_key :id

        attribute :tenant_id, :integer do
          public?(true)
        end

        attribute :name, :string do
          public?(true)
        end
      end

      def parse_tenant("org_" <> id), do: String.to_integer(id)
      def parse_tenant(id) when is_integer(id), do: id

      def format_tenant(id) when is_integer(id), do: "org_#{id}"
    end

    test "tenant_from_attribute option can be configured" do
      assert {TenantFromAttributeThing, :format_tenant, []} ==
               Ash.Resource.Info.multitenancy_tenant_from_attribute(TenantFromAttributeThing)
    end

    test "default is identity function when not specified" do
      assert {Ash.Resource.Dsl, :identity, []} ==
               Ash.Resource.Info.multitenancy_tenant_from_attribute(User)
    end

    test "round-trip conversion works with parse_attribute and tenant_from_attribute" do
      tenant = "org_42"

      {m1, f1, a1} = Ash.Resource.Info.multitenancy_parse_attribute(TenantFromAttributeThing)
      attribute_value = apply(m1, f1, [tenant | a1])
      assert attribute_value == 42

      {m2, f2, a2} =
        Ash.Resource.Info.multitenancy_tenant_from_attribute(TenantFromAttributeThing)

      tenant_result = apply(m2, f2, [attribute_value | a2])
      assert tenant_result == tenant
    end

    test "works with create and read operations" do
      tenant = "org_123"

      thing =
        TenantFromAttributeThing
        |> Ash.Changeset.for_create(:create, %{name: "test"}, tenant: tenant)
        |> Ash.create!()

      assert thing.tenant_id == 123

      results =
        TenantFromAttributeThing
        |> Ash.Query.set_tenant(tenant)
        |> Ash.read!()

      assert [%{tenant_id: 123, name: "test"}] = results

      {m, f, a} =
        Ash.Resource.Info.multitenancy_tenant_from_attribute(TenantFromAttributeThing)

      converted_tenant = apply(m, f, [thing.tenant_id | a])
      assert converted_tenant == tenant
    end
  end
end
