# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.SimpleTest do
  @doc false
  use ExUnit.Case
  require Ash.Query

  alias Ash.Test.Support.PolicySimple.{
    Car,
    Context,
    Domain,
    Foo,
    Organization,
    Post,
    Trip,
    Tweet,
    User
  }

  defmodule ResourceWithNoPolicies do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]
    end
  end

  defmodule ResourceWithAPolicyThatDoesntApply do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]
    end

    policies do
      policy never() do
        authorize_if always()
      end
    end
  end

  defmodule ResourceWithAStrictReadPolicy do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]
    end

    policies do
      policy action_type(:read) do
        access_type :strict
        authorize_if actor_attribute_equals(:admin, true)
      end

      policy action_type(:read) do
        authorize_if expr(id == ^actor(:id))
      end
    end
  end

  defmodule ResourceWithAnImpossibleCreatePolicy do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:create, :read]
    end

    policies do
      policy action(:create) do
        authorize_if expr(self.id == ^actor(:id))
      end
    end

    relationships do
      belongs_to :self, ResourceWithAnImpossibleCreatePolicy do
        filterable? true
        source_attribute :id
        destination_attribute :id
      end
    end
  end

  defmodule OldEnoughToDrink do
    use Ash.Policy.FilterCheck

    @impl true
    def describe(_opts) do
      "is old enough to drink"
    end

    @impl true
    def filter(_, _, _opts) do
      expr(age > 21)
    end
  end

  defmodule ConditionalAccessCheckBroken do
    use Ash.Policy.FilterCheck

    @impl true
    def describe(_opts) do
      "conditional access based on actor level"
    end

    @impl true
    def filter(actor, _context, _opts) do
      case actor[:level] do
        :full -> true
        :partial -> expr(access_level != :confidential)
        _ -> false
      end
    end
  end

  defmodule ConditionalAccessCheckFixed do
    use Ash.Policy.FilterCheck

    @impl true
    def describe(_opts) do
      "conditional access based on actor level"
    end

    @impl true
    def filter(actor, _context, _opts) do
      case actor[:level] do
        :full -> expr(true)
        :partial -> expr(access_level != :confidential)
        _ -> expr(false)
      end
    end
  end

  defmodule ResourceWithFailedFilterTest do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :age, :integer
    end

    actions do
      defaults [:create, :read, :update]
    end

    policies do
      policy action(:create) do
        authorize_if always()
      end

      policy action(:update) do
        authorize_if OldEnoughToDrink
        authorize_if expr(id == ^actor(:id))
        authorize_if relates_to_actor_via(:self)
      end
    end

    relationships do
      belongs_to :self, __MODULE__ do
        source_attribute :id
        destination_attribute :id
      end
    end
  end

  defmodule ResourceWithMixedActionTypePolicy do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true, allow_nil?: false
    end

    actions do
      defaults [:read, :destroy, create: [:name], update: [:name]]
    end

    policies do
      policy action_type([:create, :read, :update, :destroy]) do
        authorize_if actor_present()
      end
    end
  end

  defmodule ResourceWithBrokenConditionalCheck do
    @moduledoc """
    This resource intentionally has a trailing bypass to reproduce the bug
    where returning raw `true` from a FilterCheck causes all records to be
    filtered out when there's a trailing bypass.
    """
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :access_level, :atom, public?: true
    end

    actions do
      defaults [:read, create: [:access_level]]
    end

    policies do
      policy action_type(:read) do
        authorize_if ConditionalAccessCheckBroken
      end

      bypass actor_attribute_equals(:super_admin, true) do
        authorize_if always()
      end
    end
  end

  defmodule ResourceWithFixedConditionalCheck do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :access_level, :atom, public?: true
    end

    actions do
      defaults [:read, create: [:access_level]]
    end

    policies do
      bypass actor_attribute_equals(:super_admin, true) do
        authorize_if always()
      end

      policy action_type(:read) do
        authorize_if ConditionalAccessCheckFixed
      end
    end
  end

  setup do
    old_env = Application.get_env(:ash, :policies, [])

    Application.put_env(
      :ash,
      :policies,
      Keyword.merge(old_env, show_policy_breakdowns?: true)
    )

    on_exit(fn ->
      Application.put_env(:ash, :policies, old_env)
    end)

    [
      user: Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false),
      admin:
        Ash.create!(Ash.Changeset.for_create(User, :create, %{admin: true}), authorize?: false)
    ]
  end

  test "breakdowns for resources with no policies explain the error" do
    assert_raise Ash.Error.Forbidden,
                 ~r/No policies defined on `Ash.Test.Domain` or `Ash.Test.Policy.SimpleTest.ResourceWithNoPolicies`/,
                 fn ->
                   ResourceWithNoPolicies
                   |> Ash.read!()
                 end
  end

  test "breakdowns for action where no policies that apply explain the error" do
    assert_raise Ash.Error.Forbidden,
                 ~r/No policy conditions applied to this request/,
                 fn ->
                   ResourceWithAPolicyThatDoesntApply
                   |> Ash.read!()
                 end
  end

  test "breakdowns can be shown for all policy errors in a forbidden error" do
    res =
      assert_raise Ash.Error.Forbidden,
                   ~r/No policy conditions applied to this request/,
                   fn ->
                     ResourceWithAPolicyThatDoesntApply
                     |> Ash.read!()
                   end

    assert Ash.Error.Forbidden.Policy.report(res) =~
             ~r/No policy conditions applied to this request/
  end

  defmodule Scope do
    defstruct [:actor, :tenant, :context]

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{actor: actor}), do: {:ok, actor}
      def get_tenant(%{tenant: tenant}), do: {:ok, tenant}
      def get_context(%{context: context}), do: {:ok, context}
      def get_tracer(_), do: :error
      def get_authorize?(_), do: :error
    end
  end

  test "an impossible create policy shows the correct error message" do
    assert_raise Ash.Error.Forbidden, ~r/Cannot use a filter to authorize a create/, fn ->
      ResourceWithAnImpossibleCreatePolicy
      |> Ash.create!(%{}, actor: %{id: 10})
    end
  end

  test "a filter check shows a more in-depth breakdown of filter checks" do
    actor_id = Ash.UUID.generate()

    exception =
      assert_raise Ash.Error.Forbidden, fn ->
        ResourceWithFailedFilterTest
        |> Ash.create!(%{}, actor: %{id: actor_id})
        |> Ash.Changeset.for_update(:update, %{})
        |> Ash.update!(%{}, actor: %{id: actor_id})
      end

    message = Exception.message(exception)

    assert message =~ "Actor: %{id: \"#{actor_id}\"}"
    assert message =~ "authorize if: is old enough to drink | age > 21 | ? | 🔎"
    assert message =~ "authorize if: id == \"#{actor_id}\" | ? | 🔎"
  end

  test "strict read policies do not result in a filter" do
    thing =
      ResourceWithAStrictReadPolicy
      |> Ash.create!(authorize?: false)

    actor = %{id: thing, admin: false}

    assert_raise Ash.Error.Forbidden, fn ->
      ResourceWithAStrictReadPolicy
      |> Ash.Query.new()
      |> Ash.DataLayer.Simple.set_data([thing])
      |> Ash.read!(actor: actor)
    end

    assert [] =
             ResourceWithAStrictReadPolicy
             |> Ash.Query.new()
             |> Ash.DataLayer.Simple.set_data([%{thing | id: Ash.UUID.generate()}])
             |> Ash.read!(actor: %{admin: true})
  end

  defmodule ResourceWithBypassAndStrictPolicy do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults [:read, create: [:name]]
    end

    policies do
      # Bypass that won't apply to regular users
      bypass always() do
        authorize_if actor_attribute_equals(:admin, true)
      end

      # Strict policy that requires admin
      policy action_type(:read) do
        access_type :strict
        authorize_if actor_attribute_equals(:admin, true)
      end
    end
  end

  test "strict policy without matching bypass returns forbidden instead of empty list", %{
    user: user,
    admin: admin
  } do
    # Create some records
    ResourceWithBypassAndStrictPolicy
    |> Ash.Changeset.for_create(:create, %{name: "test1"})
    |> Ash.create!(authorize?: false)

    ResourceWithBypassAndStrictPolicy
    |> Ash.Changeset.for_create(:create, %{name: "test2"})
    |> Ash.create!(authorize?: false)

    # Admin should be able to read via bypass
    assert [_, _] = Ash.read!(ResourceWithBypassAndStrictPolicy, actor: admin)

    # Non-admin user should get forbidden error, NOT empty list
    # This reproduces the issue where strict policies were incorrectly
    # returning 200 with empty data instead of 403 forbidden
    assert_raise Ash.Error.Forbidden, fn ->
      ResourceWithBypassAndStrictPolicy
      |> Ash.read!(actor: user)
    end
  end

  test "bypass with condition does not apply subsequent filters", %{admin: admin, user: user} do
    Ash.create!(Ash.Changeset.for_create(Tweet, :create), authorize?: false)

    assert [_] = Ash.read!(Tweet, actor: admin)
    assert [] = Ash.read!(Tweet, actor: user)
  end

  test "Ash.can? accepts a record to determine if it can be read", %{admin: admin, user: user} do
    tweet = Ash.create!(Ash.Changeset.for_create(Tweet, :create), authorize?: false)

    assert Ash.can?({Ash.Query.new(Tweet), :read}, admin)
    assert Ash.can?({Ash.Query.new(Tweet), :read, %{}}, user)

    assert Ash.can?({tweet, :read}, admin)
    refute Ash.can?({tweet, :read}, user)
  end

  test "Ash.can? honors a provided scope", %{admin: admin, user: user} do
    tweet = Ash.create!(Ash.Changeset.for_create(Tweet, :create), authorize?: false)
    assert Ash.can?({tweet, :read}, %Scope{actor: admin})
    refute Ash.can?({tweet, :read}, %Scope{actor: user})
  end

  test "arguments can be referenced in expression policies", %{admin: admin, user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create_foo, %{foo: "foo", user_id: admin.id}, actor: user)
    |> Ash.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create_foo, %{foo: "bar", user_id: admin.id}, actor: user)
      |> Ash.create!()
    end
  end

  test "functions can be used as checks through `matches`", %{user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create_bar, %{bar: 2}, actor: user)
    |> Ash.create!()

    Tweet
    |> Ash.Changeset.for_create(:create_bar, %{bar: 9}, actor: user)
    |> Ash.create!()

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create_bar, %{bar: 1}, actor: user)
      |> Ash.create!()
    end
  end

  test "filter checks work on create/update/destroy actions", %{user: user} do
    user2 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.update!(Ash.Changeset.for_update(user, :update), actor: user2)
    end
  end

  test "relating_to_actor/1 works when creating", %{user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Ash.create!(authorize?: true, actor: user)

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create, %{user_id: Ash.UUID.generate()})
      |> Ash.create!(authorize?: true, actor: user)
    end
  end

  test "scope is honored", %{user: user} do
    scope = %Scope{
      actor: user,
      tenant: nil,
      context: %{}
    }

    Tweet
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Ash.create!(authorize?: true, scope: scope)

    assert_raise Ash.Error.Forbidden, fn ->
      Tweet
      |> Ash.Changeset.for_create(:create, %{user_id: Ash.UUID.generate()})
      |> Ash.create!(authorize?: true, scope: scope)
    end
  end

  test "relating_to_actor/1 works when updating", %{user: user} do
    Tweet
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Ash.create!(authorize?: false, actor: user)

    Ash.bulk_update!(Tweet, :set_user, %{user_id: user.id},
      actor: user,
      authorize?: true,
      authorize_with: :error
    )

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.bulk_update!(Tweet, :set_user, %{user_id: Ash.UUID.generate()},
        actor: user,
        authorize?: true,
        authorize_with: :error
      )
    end
  end

  test "relating_to_actor/1 works when updating non-atomically", %{user: user} do
    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create, %{user_id: user.id})
      |> Ash.create!(authorize?: false, actor: user)

    tweet
    |> Ash.Changeset.for_update(:set_user, %{user_id: user.id}, actor: user, authorize?: true)
    |> Ash.update!()

    assert_raise Ash.Error.Forbidden, fn ->
      tweet
      |> Ash.Changeset.for_update(:set_user, %{user_id: Ash.UUID.generate()},
        actor: user,
        authorize?: true
      )
      |> Ash.update!()
    end
  end

  test "filter checks work on update/destroy actions", %{user: user} do
    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Ash.Changeset.manage_relationship(:user, user, type: :append_and_remove)
      |> Ash.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(user, changeset, Domain) == true

    tweet =
      Tweet
      |> Ash.Changeset.for_create(:create)
      |> Ash.create!(authorize?: false)

    changeset = Ash.Changeset.for_update(tweet, :update)

    assert Ash.Policy.Info.strict_check(%{user | id: nil}, changeset, Domain) == false
  end

  test "non-filter checks work on create/update/destroy actions" do
    user = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{text: "foo"}), actor: user)
    end
  end

  test "filter checks work with related data", %{user: user} do
    organization =
      Organization
      |> Ash.Changeset.for_create(:create, %{owner: user.id})
      |> Ash.create!(authorize?: false)

    post1 =
      Post
      |> Ash.Changeset.for_create(:create, %{author: user.id, text: "aaa"})
      |> Ash.create!(authorize?: false)

    post2 =
      Post
      |> Ash.Changeset.for_create(:create, %{organization: organization.id, text: "bbb"})
      |> Ash.create!(authorize?: false)

    Post
    |> Ash.Changeset.for_create(:create, %{text: "invalid"})
    |> Ash.create!(authorize?: false)

    ids =
      Post
      |> Ash.read!(actor: user)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert ids == Enum.sort([post1.id, post2.id])
  end

  test "authorize_with `:error` is an error if any records don't match", %{user: user} do
    post1 =
      Post
      |> Ash.Changeset.for_create(:create, %{author: user.id, text: "aaa"})
      |> Ash.create!(authorize?: false)

    post2 =
      Post
      |> Ash.Changeset.for_create(:create, %{text: "invalid"})
      |> Ash.create!(authorize?: false)

    ids =
      Post
      |> Ash.read!(actor: user)
      |> Enum.map(& &1.id)
      |> Enum.sort()

    assert ids == Enum.sort([post1.id])

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.read!(Post, actor: user, authorize_with: :error)
    end

    Ash.get!(Post, post1.id, actor: user, authorize_with: :error)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.get!(Post, post2.id, actor: user, authorize_with: :error)
    end
  end

  test "filter policies bypassed for calculations", %{user: user} do
    other_user = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    Post
    |> Ash.Changeset.for_create(:create, %{author: user.id, text: "aaa"})
    |> Ash.create!(authorize?: false)

    assert %{post_texts: ["aaa"]} =
             Ash.load!(user, :post_texts, actor: other_user)
  end

  test "authorize_unless properly combines", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:authorize_unless, %{})
    |> Ash.create!(actor: user)
  end

  test "filter checks work on generic actions when they don't reference anything specifically", %{
    admin: admin,
    user: user
  } do
    assert_raise Ash.Error.Forbidden, fn ->
      Post
      |> Ash.ActionInput.for_action(:say_hello, %{from_an_admin?: true, to: "Fred"}, actor: user)
      |> Ash.run_action!()
    end

    assert "Hello Fred from an admin!" ==
             Post
             |> Ash.ActionInput.for_action(:say_hello, %{from_an_admin?: true, to: "Fred"},
               actor: admin
             )
             |> Ash.run_action!()
  end

  test "filter checks work with many to many related data and a filter", %{user: user} do
    car1 =
      Car
      |> Ash.Changeset.for_create(:create, %{users: [user.id]})
      |> Ash.create!(authorize?: false)

    car2 =
      Car
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!(authorize?: false)

    results =
      Car
      |> Ash.Query.filter(id == ^car2.id)
      |> Ash.read!(actor: user)

    assert results == []

    results =
      Car
      |> Ash.Query.filter(id == ^car1.id)
      |> Ash.read!(actor: user)
      |> Enum.map(& &1.id)

    assert results == [car1.id]
  end

  test "count is skipped when no records are returned due to policies" do
    car =
      Car
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!(authorize?: false)

    Mimic.reject(Ash.DataLayer, :run_query, 2)
    Mimic.reject(Ash.DataLayer, :run_aggregate_query, 3)

    assert %{count: 0, results: []} =
             Ash.Query.for_read(Car, :with_pagination)
             |> Ash.Query.filter(id == ^car.id)
             |> Ash.Query.page(limit: 5, count: true)
             |> Ash.read!()
  end

  test "calculations that reference aggregates are properly authorized", %{user: user} do
    Car
    |> Ash.Changeset.for_create(:create, %{users: [user.id], active: false}, authorize?: false)
    |> Ash.create!()

    assert %{restricted_from_driving: false, has_car: true} =
             user
             |> Ash.load!([:restricted_from_driving, :has_car], authorize?: false)
             |> Map.take([:restricted_from_driving, :has_car])

    assert %{restricted_from_driving: true, has_car: false} =
             user
             |> Ash.load!([:restricted_from_driving, :has_car], authorize?: true)
             |> Map.take([:restricted_from_driving, :has_car])
  end

  test "filter checks work via deeply related data", %{user: user} do
    assert Ash.read!(Trip, actor: user) == []
  end

  test "changing_attributes with `:to` option works" do
    Foo
    |> Ash.Changeset.for_create(:create, %{name: "Foo"})
    |> Ash.create!()
  end

  test "checking context using expr works" do
    %{id: id} =
      context =
      Context
      |> Ash.Changeset.for_create(:create, %{name: "Foo"})
      |> Ash.create!()

    assert [%{id: ^id}] = Ash.read!(Context, context: %{name: "Foo"}, authorize?: true)

    assert %{name: "Bar"} =
             context
             |> Ash.Changeset.for_update(:update, %{name: "Bar"},
               context: %{name: "Foo"},
               authorize?: true
             )
             |> Ash.update!()

    assert %{name: "Foo"} =
             Domain.update_context!(id, "Foo",
               context: %{name: "Bar"},
               actor: nil,
               authorize?: true
             )
  end

  test "a final always policy with a forbid if always is properly applied" do
    user = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    Ash.Test.Support.PolicySimple.Always
    |> Ash.Changeset.for_create(:create, %{user_id: user.id})
    |> Ash.create!(authorize?: false)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.Test.Support.PolicySimple.Always
      |> Ash.read!(authorize?: true, actor: user)
    end
  end

  test "two filter condition checks combine properly" do
    user1 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)
    user2 = Ash.create!(Ash.Changeset.for_create(User, :create), authorize?: false)

    user1_thing =
      Ash.Test.Support.PolicySimple.TwoFilters
      |> Ash.Changeset.for_create(:create, %{user_id: user1.id})
      |> Ash.create!(authorize?: false)

    user2_thing =
      Ash.Test.Support.PolicySimple.TwoFilters
      |> Ash.Changeset.for_create(:create, %{user_id: user2.id})
      |> Ash.create!(authorize?: false)

    assert [user1_got_thing] =
             Ash.Test.Support.PolicySimple.TwoFilters
             |> Ash.read!(authorize?: true, actor: user1)

    assert user1_got_thing.id == user1_thing.id

    assert [user2_got_thing] =
             Ash.Test.Support.PolicySimple.TwoFilters
             |> Ash.read!(authorize?: true, actor: user2)

    assert user2_got_thing.id == user2_thing.id
  end

  defmodule ResourceWithBypassAndReadPolicy do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults [:read]

      create :create do
        primary? true
        accept [:name]
      end
    end

    policies do
      bypass always() do
        authorize_if actor_attribute_equals(:admin, true)
      end

      policy action_type(:read) do
        authorize_if always()
      end
    end
  end

  defmodule ResourceWithBeforeTransactionHook do
    use Ash.Resource,
      domain: Ash.Test.Domain,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults [:read]

      create :create do
        primary? true
        accept [:name]
      end

      update :test_action_with_after_transaction do
        accept [:name]

        change before_transaction(fn changeset, _context ->
                 raise "running before transaction"
               end)

        change after_transaction(fn _changeset, res, _context ->
                 res
               end)

        require_atomic? false
      end

      update :test_action_without_after_transaction do
        accept [:name]

        change before_transaction(fn changeset, _context ->
                 raise "running before transaction"
               end)

        require_atomic? false
      end
    end

    policies do
      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if always()
      end
    end
  end

  test "bypass with only admin check should not allow non-admin creates", %{
    user: user,
    admin: admin
  } do
    # Non-admin user should not be able to create
    assert_raise Ash.Error.Forbidden, fn ->
      ResourceWithBypassAndReadPolicy
      |> Ash.Changeset.for_create(:create, %{name: "test"})
      |> Ash.create!(actor: user)
    end

    # Admin should be able to create via bypass
    assert %{name: "admin_record"} =
             ResourceWithBypassAndReadPolicy
             |> Ash.Changeset.for_create(:create, %{name: "admin_record"})
             |> Ash.create!(actor: admin)

    # Both admin and non-admin should be able to read
    assert [_] = Ash.read!(ResourceWithBypassAndReadPolicy, actor: admin)
    assert [_] = Ash.read!(ResourceWithBypassAndReadPolicy, actor: user)
  end

  test "before_transaction hook should not run when action is not authorized via bulk_update" do
    record = Ash.create!(ResourceWithBeforeTransactionHook, %{name: "test"}, authorize?: false)

    query =
      ResourceWithBeforeTransactionHook
      |> Ash.DataLayer.Simple.set_data([record])
      |> Ash.Query.limit(1)

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.bulk_update!(query, :test_action_with_after_transaction, %{},
        return_errors?: true,
        notify?: true,
        strategy: [:atomic, :stream, :atomic_batches],
        allow_stream_with: :full_read,
        authorize_changeset_with: :filter,
        return_records?: true,
        authorize?: true,
        read_action: :read,
        domain: Ash.Test.Domain,
        select: [],
        load: []
      )
    end

    assert_raise Ash.Error.Forbidden, fn ->
      Ash.bulk_update!(query, :test_action_without_after_transaction, %{},
        return_errors?: true,
        notify?: true,
        strategy: [:atomic, :stream, :atomic_batches],
        allow_stream_with: :full_read,
        authorize_changeset_with: :filter,
        return_records?: true,
        authorize?: true,
        read_action: :read,
        domain: Ash.Test.Domain,
        select: [],
        load: []
      )
    end
  end

  describe "ResourceWithMixedActionTypePolicy" do
    test "when an actor is provided, the data is returned" do
      data =
        ResourceWithMixedActionTypePolicy
        |> Ash.Changeset.for_create(:create, %{name: "Joe"}, actor: :bob, authorize?: false)
        |> Ash.create!()
        |> List.wrap()

      assert [%{name: "Joe"}] =
               ResourceWithMixedActionTypePolicy
               |> Ash.DataLayer.Simple.set_data(data)
               |> Ash.read!(actor: :bob)
    end

    test "when no actor is provided, filtering is applied as expected" do
      data =
        ResourceWithMixedActionTypePolicy
        |> Ash.Changeset.for_create(:create, %{name: "Joe"}, actor: :bob, authorize?: false)
        |> Ash.create!()
        |> List.wrap()

      assert [] =
               ResourceWithMixedActionTypePolicy
               |> Ash.DataLayer.Simple.set_data(data)
               |> Ash.read!()
    end
  end

  describe "FilterCheck with trailing bypass bug" do
    test "returning raw true/false from FilterCheck fails with trailing bypass" do
      public_record =
        ResourceWithBrokenConditionalCheck
        |> Ash.Changeset.for_create(:create, %{access_level: :public}, authorize?: false)
        |> Ash.create!()

      confidential_record =
        ResourceWithBrokenConditionalCheck
        |> Ash.Changeset.for_create(:create, %{access_level: :confidential}, authorize?: false)
        |> Ash.create!()

      full_actor = %{level: :full}

      full_results =
        ResourceWithBrokenConditionalCheck
        |> Ash.read!(actor: full_actor)
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert full_results == Enum.sort([public_record.id, confidential_record.id])

      partial_actor = %{level: :partial}

      partial_results =
        ResourceWithBrokenConditionalCheck
        |> Ash.read!(actor: partial_actor)
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert partial_results == [public_record.id]
    end

    test "returning expr(true) and expr(false) from FilterCheck works correctly with trailing bypass" do
      # Create test records
      public_record =
        ResourceWithFixedConditionalCheck
        |> Ash.Changeset.for_create(:create, %{access_level: :public}, authorize?: false)
        |> Ash.create!()

      confidential_record =
        ResourceWithFixedConditionalCheck
        |> Ash.Changeset.for_create(:create, %{access_level: :confidential}, authorize?: false)
        |> Ash.create!()

      full_actor = %{level: :full}

      full_results =
        ResourceWithFixedConditionalCheck
        |> Ash.read!(actor: full_actor)
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert full_results == Enum.sort([public_record.id, confidential_record.id])

      partial_actor = %{level: :partial}

      partial_results =
        ResourceWithFixedConditionalCheck
        |> Ash.read!(actor: partial_actor)
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert partial_results == [public_record.id]

      no_access_actor = %{level: :none}

      no_results = Ash.read!(ResourceWithFixedConditionalCheck, actor: no_access_actor)
      assert no_results == []

      super_admin_actor = %{super_admin: true, level: :none}

      super_admin_results =
        ResourceWithFixedConditionalCheck
        |> Ash.read!(actor: super_admin_actor)
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert super_admin_results == Enum.sort([public_record.id, confidential_record.id])
    end
  end
end
