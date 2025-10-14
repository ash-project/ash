# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.CodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  doctest Ash.CodeInterface, import: true

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
    end
  end

  defmodule NonStreamable do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets, notifiers: [Notifier]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults create: [:name]

      update :update do
        primary? true
        require_atomic? false
        accept [:name]

        change fn changeset, _ ->
          changeset
        end
      end

      read :read do
        primary? true
      end
    end

    code_interface do
      define :update
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets, notifiers: [Notifier]

    ets do
      private?(true)
    end

    code_interface do
      define :get_user, action: :read, get_by: :id
      define :get_user_safely, action: :read, get_by: :id, not_found_error?: false
      define :read_users, action: :read
      define :get_by_id, action: :by_id, get?: true, args: [:id]
      define :update_by_id, action: :update_by_id_without_filter, get?: true, args: [:id]
      define :destroy_by_id, action: :destroy_by_id_without_filter, get?: true, args: [:id]
      define :create, args: [{:optional, :first_name}]
      define :insert, action: :create
      define :hello, args: [:name]

      define :update_by_id_map do
        action :update_by_id_without_filter
        get? true
        args [:map]

        custom_input :map, :map do
          transform to: :id, using: &Map.fetch!(&1, :id)
        end
      end

      define :hello_excluded do
        action :hello
        exclude_inputs([:name])
      end

      define :hello_to_user do
        action :hello
        args [:user]

        custom_input :user, :map do
          allow_nil? false
          transform to: :name, using: &Map.fetch!(&1, :name)
        end
      end

      define :hello_actor_with_default,
        default_options: [actor: %{name: "William Shatner"}],
        action: :hello_actor

      define :hello_actor_with_function_default,
        default_options: fn ->
          [actor: %{name: "Dynamic Actor at #{DateTime.utc_now() |> DateTime.to_iso8601()}"}]
        end,
        action: :hello_actor

      define :get_user_with_invalid_load,
        default_options: [load: [[invalid: :load]]],
        action: :read,
        get_by: :id

      define :hello_actor
      define :create_with_map, args: [:map]
      define :update_with_map, args: [:map]

      define :bulk_create, action: :create
      define :update, action: :update
      define :destroy, action: :destroy
      define :create_with_private, action: :create_with_private
      define :update_with_private, action: :update_with_private
      define :atomic_update_with_private, action: :atomic_update_with_private
      define :destroy_with_private, action: :destroy_with_private
      define :soft_destroy_with_private, action: :soft_destroy_with_private

      define_calculation(:full_name, args: [:first_name, :last_name])

      define_calculation(:full_name_excluded,
        exclude_inputs: [:first_name],
        calculation: :full_name
      )

      define_calculation(:full_name_opt,
        calculation: :full_name,
        args: [:first_name, :last_name, {:optional, :separator}]
      )

      define_calculation(:full_name_record, calculation: :full_name, args: [:_record])

      define_calculation(:full_name_functional_record,
        calculation: :full_name_functional,
        args: [:_record]
      )
    end

    actions do
      default_accept :*

      read :read do
        primary? true
        pagination keyset?: true, required?: false
      end

      create :create

      create :create_with_map do
        argument :map, :map, allow_nil?: false
      end

      update :update

      update :update_with_map do
        argument :map, :map, allow_nil?: false
      end

      destroy :destroy

      read :by_id do
        argument :id, :uuid, allow_nil?: false

        filter expr(id == ^arg(:id))
      end

      update :update_by_id_without_filter do
        argument :id, :string, allow_nil?: false
        # missing filter
      end

      destroy :destroy_by_id_without_filter do
        argument :id, :string, allow_nil?: false
        # missing filter
      end

      action :hello, :string do
        argument :name, :string, allow_nil?: false

        run(fn input, _ ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
      end

      action :hello_actor, :string do
        run(fn input, context ->
          {:ok, "Hello, #{context.actor.name}."}
        end)
      end

      create :create_with_private do
        argument :private_tag, :string, allow_nil?: false, public?: false
        accept [:first_name, :last_name]

        change fn changeset, _ ->
          Ash.Changeset.change_attribute(
            changeset,
            :last_name,
            "#{changeset.arguments.private_tag}_tagged"
          )
        end
      end

      update :update_with_private do
        require_atomic? false
        argument :private_flag, :string, allow_nil?: false, public?: false
        accept [:first_name, :last_name]

        change fn changeset, _ ->
          Ash.Changeset.change_attribute(
            changeset,
            :last_name,
            "#{changeset.arguments.private_flag}_flagged"
          )
        end
      end

      update :atomic_update_with_private do
        require_atomic? false
        argument :private_flag, :string, allow_nil?: false, public?: false
        accept [:first_name, :last_name]

        change atomic_update(:last_name, expr(^arg(:private_flag) <> "_flagged"))
      end

      destroy :destroy_with_private do
        argument :private_reason, :string, allow_nil?: false, public?: false
        accept []
      end

      destroy :soft_destroy_with_private do
        require_atomic? false
        soft? true
        argument :private_reason, :string, allow_nil?: false, public?: false
        accept []

        change fn changeset, _ ->
          changeset
          |> Ash.Changeset.change_attribute(:deleted_at, DateTime.utc_now())
          |> Ash.Changeset.change_attribute(
            :last_name,
            "soft_destroyed_by_#{changeset.arguments.private_reason}"
          )
        end
      end
    end

    calculations do
      calculate :full_name, :string, expr(first_name <> ^arg(:separator) <> last_name) do
        public?(true)

        argument :separator, :string,
          default: " ",
          allow_nil?: false,
          constraints: [allow_empty?: true, trim?: false]
      end

      calculate :full_name_functional,
                :string,
                fn records, _ ->
                  Enum.map(records, fn record ->
                    record.first_name <> " " <> record.last_name
                  end)
                end,
                load: [:first_name, :last_name]
    end

    attributes do
      uuid_primary_key :id

      attribute :first_name, :string do
        public?(true)
        allow_nil? false
        default "fred"
      end

      attribute :last_name, :string do
        public?(true)
      end

      attribute :deleted_at, :utc_datetime do
        public?(true)
      end
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource User do
        define :get_user, action: :read, get_by: :id

        define :hello_to_user do
          action :hello
          args [:user]

          custom_input :user, :map do
            allow_nil? false
            transform to: :name, using: &Map.fetch!(&1, :name)
          end
        end

        define_calculation(:full_name, args: [:first_name, :last_name])
      end
    end
  end

  @context %{test: "value"}

  setup do
    Application.put_env(Notifier, :notifier_test_pid, self())

    :ok
  end

  describe "exclude_inputs" do
    test "raises an error when used" do
      assert_raise ArgumentError,
                   ~r/`name` not accepted by Ash\.Test\.CodeInterfaceTest\.User\.hello_excluded\/2/,
                   fn -> User.hello_excluded(%{name: "fred"}) end

      assert_raise ArgumentError,
                   ~r/`first_name` not accepted by Ash\.Test\.CodeInterfaceTest\.User\.full_name\/1/,
                   fn ->
                     User.full_name_excluded!(args: %{first_name: "Zach"})
                   end
    end
  end

  describe "custom_input" do
    test "exclude inputs from required positional arguments" do
      assert_raise ArgumentError, ~r/`name` not accepted/, fn ->
        User.hello_to_user(%{name: "name"}, %{name: "name"})
      end

      assert_raise ArgumentError, ~r/`name` not accepted/, fn ->
        Domain.hello_to_user(%{name: "name"}, %{name: "name"})
      end
    end

    test "are validated" do
      assert_raise Ash.Error.Invalid, ~r/custom_input user is required/, fn ->
        User.hello_to_user!(nil)
      end

      assert_raise Ash.Error.Invalid, ~r/custom_input user is required/, fn ->
        Domain.hello_to_user!(nil)
      end

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for map: is invalid/, fn ->
        User.update_by_id_map!(10)
      end

      assert_raise Ash.Error.Invalid, ~r/custom_input user is required/, fn ->
        Domain.hello_to_user!(nil)
      end
    end

    test "are suppled and transformed" do
      assert "Hello name" == User.hello_to_user!(%{name: "name"})
      assert "Hello name" == Domain.hello_to_user!(%{name: "name"})
    end
  end

  describe "generic actions" do
    test "can be invoked" do
      assert "Hello fred" == User.hello!("fred")
      assert {:ok, "Hello george"} == User.hello("george")
    end

    test "have a helper method to produce inputs" do
      assert %Ash.ActionInput{action: %{name: :hello}, params: %{name: "bob"}} =
               User.input_to_hello("bob")
    end

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_hello(nil, "fred")
      assert User.can_hello?(nil, "fred")
    end
  end

  describe "read actions" do
    test "have a helper methods to produce queries" do
      assert %Ash.Query{action: %{name: :read}} = User.query_to_read_users()
      assert %Ash.Query{action: %{name: :by_id}} = User.query_to_get_by_id("some uuid")
    end

    test "results can be sorted" do
      User.create!("bob")
      User.create!("cob")

      assert [%{first_name: "bob"}, %{first_name: "cob"}] =
               User.read_users!(query: [sort: [first_name: :asc]], authorize?: false)

      assert [%{first_name: "cob"}, %{first_name: "bob"}] =
               User.read_users!(query: [sort: [first_name: :desc]], authorize?: false)
    end

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_read_users(nil)
      assert {:ok, true} == User.can_get_by_id(nil, "some uuid")
      assert User.can_read_users?(nil)
      assert User.can_get_by_id?(nil, "some uuid")
    end
  end

  describe "read get actions" do
    test "raise on not found by default" do
      assert_raise Ash.Error.Invalid, ~r/not found/, fn ->
        User.get_user!(Ash.UUID.generate())
      end

      assert_raise Ash.Error.Invalid, ~r/not found/, fn ->
        Domain.get_user!(Ash.UUID.generate())
      end
    end

    test "do not raise on not found if were defined with `not_found_error?: false`" do
      assert nil == User.get_user_safely!(Ash.UUID.generate())
    end

    test "do not raise on not found if option `not_found_error?: false` is passed" do
      assert nil == User.get_user!(Ash.UUID.generate(), not_found_error?: false)
      assert nil == Domain.get_user!(Ash.UUID.generate(), not_found_error?: false)
      assert nil == User.get_user_safely!(Ash.UUID.generate(), not_found_error?: false)
    end

    test "raise on not found if option `not_found_error?: true` is passed" do
      assert_raise Ash.Error.Invalid, ~r/not found/, fn ->
        User.get_user_safely!(Ash.UUID.generate(), not_found_error?: true)
      end
    end

    test "can take a @context options" do
      assert {:ok, nil} ==
               User.get_user(Ash.UUID.generate(), not_found_error?: false, context: @context)

      assert nil ==
               User.get_user!(Ash.UUID.generate(), not_found_error?: false, context: @context)
    end
  end

  describe "create actions" do
    test "have a helper methods to produce changeset" do
      assert %Ash.Changeset{action: %{name: :create}, attributes: %{first_name: "fred"}} =
               User.changeset_to_create()

      assert %Ash.Changeset{action: %{name: :create}, attributes: %{first_name: "bob"}} =
               User.changeset_to_create("bob")
    end

    test "can handle when a map is provided as an argument value" do
      User.create_with_map!(%{some_random_key: 10})
    end

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_create(nil)
      assert {:ok, true} == User.can_create(nil, "bob")
      assert User.can_create?(nil)
      assert User.can_create?(nil, "bob")
    end

    test "can take a @context options" do
      assert {:ok, _record} = User.create("bob", context: @context)
      assert _record = User.create!("bob", context: @context)
    end

    test "bulk_create can take a @context options" do
      assert %Ash.BulkResult{status: :success} =
               User.bulk_create([%{first_name: "bob"}, %{first_name: "other_bob"}],
                 context: @context
               )
    end

    test "bulk_create can be take an empty list" do
      assert_raise ArgumentError, ~r/Cannot provide an empty list for params/, fn ->
        # Note, this is an unfortunately required breaking change
        # We can't tell the difference between an empty set of inputs to create one record w/o inputs
        # and an empty list to create no records as a bulk action
        User.bulk_create!([])
      end

      assert %Ash.BulkResult{status: :success} = User.bulk_create!([], [])
    end
  end

  describe "update actions" do
    test "have a helper methods to produce changeset" do
      bob = User.create!("bob", context: @context)

      assert %Ash.Changeset{action: %{name: :update}, attributes: %{first_name: "fred"}} =
               User.changeset_to_update(bob, %{first_name: "fred"})
    end

    test "can handle when a map is provided as an argument value" do
      bob = User.create!("bob", context: @context)
      User.update_with_map!(bob, %{some_random_key: 10})
    end

    test "can take a @context options" do
      bob = User.create!("bob", context: @context)

      assert {:ok, _record} = User.update(bob, %{first_name: "bob_updated"}, context: @context)
      assert _record = User.update!(bob, %{first_name: "bob_updated"}, context: @context)
    end

    test "can take a query" do
      bob = User.create!("bob", context: @context)
      require Ash.Query

      User
      |> Ash.Query.filter(id == ^bob.id)
      |> User.update!(%{first_name: "bob_updated"})
    end

    test "non-streamable actions can be used when limit: 1" do
      record =
        NonStreamable
        |> Ash.create!(%{name: "name"})

      record.id
      |> NonStreamable.update!(%{name: "new name"})
    end

    test "can take an id" do
      bob = User.create!("bob", context: @context)
      require Ash.Query

      assert_received {:notification, %Ash.Notifier.Notification{}}

      bob = User.update!(bob.id, %{first_name: "bob_updated"})

      assert bob.first_name == "bob_updated"

      assert_received {:notification,
                       %Ash.Notifier.Notification{resource: User, action: %{name: :update}}}
    end

    test "bulk update can take a @context options" do
      bob1 = User.create!("bob", context: @context)
      bob2 = User.create!("bob", context: @context)

      assert %Ash.BulkResult{status: :success} =
               User.update([bob1, bob2], %{first_name: "other_bob"}, context: @context)

      assert result =
               User.update!([bob1, bob2], %{first_name: "different_bob"},
                 context: @context,
                 bulk_options: [return_records?: true]
               )

      Enum.map(result.records, &assert(&1.first_name == "different_bob"))
    end

    test "bulk update can take an empty list" do
      assert %Ash.BulkResult{status: :success} = User.update([], %{first_name: "other_bob"})
    end

    test "bulk update can take an empty list and keyword params" do
      assert %Ash.BulkResult{status: :success} =
               User.update([], [first_name: "other_bob"], [])
    end
  end

  describe "destroy actions" do
    test "have a helper methods to produce changeset" do
      bob = User.create!("bob", context: @context)

      assert %Ash.Changeset{action: %{name: :destroy}} = User.changeset_to_destroy(bob)
    end

    test "can take a @context options" do
      bob1 = User.create!("bob", context: @context)
      bob2 = User.create!("bob", context: @context)

      assert :ok = User.destroy(bob1, context: @context)
      assert :ok = User.destroy!(bob2, context: @context)
    end

    test "bulk destroy can take a @context options" do
      bob1 = User.create!("bob", context: @context)
      bob2 = User.create!("bob", context: @context)

      assert %Ash.BulkResult{status: :success} = User.destroy([bob1, bob2], context: @context)

      bob3 = User.create!("bob", context: @context)
      bob4 = User.create!("bob", context: @context)

      assert %Ash.BulkResult{status: :success} = User.destroy!([bob3, bob4], context: @context)
    end

    test "bulk destroy can take an id" do
      bob1 = User.create!("bob", context: @context)
      User.destroy!(bob1.id)
    end

    test "bulk destroy can take an empty list" do
      assert %Ash.BulkResult{status: :success} = User.destroy([])
    end
  end

  describe "calculations" do
    test "calculation value can be fetched dynamically" do
      assert {:ok, "Zach Daniel"} =
               Ash.calculate(User, :full_name, refs: %{first_name: "Zach", last_name: "Daniel"})
    end

    test "the same calculation can be fetched with the calculation interface" do
      assert "Zach Daniel" = User.full_name!("Zach", "Daniel")
      assert "Zach Daniel" = Domain.full_name!("Zach", "Daniel")
    end

    test "the same calculation can be fetched with the calculation interface with optional" do
      assert "Zach Daniel" = User.full_name_opt!("Zach", "Daniel")
      assert "Zach-Daniel" = User.full_name_opt!("Zach", "Daniel", "-")
    end

    test "the calculation can accept a record" do
      user =
        User
        |> Ash.Changeset.for_create(:create, %{first_name: "Zach", last_name: "Daniel"})
        |> Ash.create!()

      assert "Zach Daniel" = User.full_name_record!(user)
      assert "Zach Daniel" = User.full_name_functional_record!(user)
    end
  end

  test "get_by! adds the proper arguments and filters" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "ted", last_name: "Danson"})
      |> Ash.create!()

    assert User.get_by_id!(user.id).id == user.id

    assert {:ok, user} = User.get_by_id(user.id)
    assert user.id == user.id
  end

  test "update/destroy returns error for get? true and missing filter" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "ted", last_name: "Danson"})
      |> Ash.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "fred", last_name: "Danson"})
    |> Ash.create!()

    assert_raise Ash.Error.Invalid, fn ->
      User.update_by_id!(user.id, %{})
    end

    assert_raise Ash.Error.Invalid, fn ->
      User.destroy_by_id!(user.id, %{})
    end
  end

  test "update/destroy with `get? true` work with `can_`" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "ted", last_name: "Danson"})
      |> Ash.create!()

    User
    |> Ash.Changeset.for_create(:create, %{first_name: "fred", last_name: "Danson"})
    |> Ash.create!()

    assert User.can_update_by_id?(user, user.id, %{})
    assert User.can_destroy_by_id?(user, user.id, %{})
  end

  test "optional arguments are optional" do
    assert User.create!().first_name == "fred"
    assert User.create!("joe").first_name == "joe"
  end

  test "it handles keyword inputs properly" do
    assert {:ok, %{last_name: "weasley"}} =
             User.create("fred", [last_name: "weasley"], actor: nil)

    assert %{last_name: "weasley"} = User.create!("fred", [last_name: "weasley"], actor: nil)
  end

  test "default options" do
    assert "Hello, Leonard Nimoy." =
             User.hello_actor_with_default!(actor: %{name: "Leonard Nimoy"})

    assert "Hello, William Shatner." = User.hello_actor_with_default!()
  end

  test "default options with function" do
    assert "Hello, Override Actor." =
             User.hello_actor_with_function_default!(actor: %{name: "Override Actor"})

    result = User.hello_actor_with_function_default!()
    assert result =~ "Hello, Dynamic Actor at "
    # ISO8601 timestamp format contains 'T'
    assert result =~ "T"
    # UTC timestamps end with 'Z'
    assert result =~ "Z"
  end

  test "default options with invalid load returns error" do
    user = User.create!("john")

    assert {:error, %Ash.Error.Invalid{errors: [error]}} =
             User.get_user_with_invalid_load(user.id)

    assert %Ash.Error.Query.InvalidLoad{load: {:invalid, :load}} = error
  end

  test "default options with function are called each time" do
    result1 = User.hello_actor_with_function_default!()
    result2 = User.hello_actor_with_function_default!()

    assert result1 =~ "Hello, Dynamic Actor at "
    assert result2 =~ "Hello, Dynamic Actor at "

    # They should have different timestamps, proving the function was called each time
    refute result1 == result2

    # Test that function-based defaults can still be overridden
    assert "Hello, Override Actor." =
             User.hello_actor_with_function_default!(actor: %{name: "Override Actor"})
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

  defmodule ContextScope do
    defstruct [:current_user, :org_id, :product_id]

    defimpl Ash.Scope.ToOpts do
      def get_actor(%{current_user: current_user}), do: {:ok, current_user}
      def get_tenant(_), do: {:ok, nil}

      def get_context(%{org_id: org_id, product_id: product_id}),
        do: {:ok, %{shared: %{org_id: org_id, product_id: product_id}}}

      def get_tracer(_), do: :error

      def get_authorize?(_), do: :error
    end
  end

  defmodule LogEvent do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    code_interface do
      define :create, args: [:event]
    end

    attributes do
      uuid_primary_key :id
      attribute(:user_id, :uuid, public?: true)
      attribute(:org_id, :uuid, public?: true)
      attribute(:product_id, :integer, public?: true)
      attribute(:event, :string, public?: true)
    end

    actions do
      defaults [:read]

      create :create do
        accept [:event]

        change set_attribute(:user_id, actor(:id))
        change set_attribute(:org_id, context(:org_id))
        change set_attribute(:product_id, context(:product_id))
      end
    end
  end

  defmodule ContextUser do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    code_interface do
      define :enable_product, args: [:product_id]
    end

    actions do
      create :create do
        primary? true
        accept [:last_used_product_id]
      end

      update :enable_product do
        require_atomic? false

        argument :product_id, :integer

        change set_attribute(:last_used_product_id, arg(:product_id))

        change after_action(fn changeset, user, context ->
                 LogEvent.create!(
                   "product enabled",
                   scope: context,
                   context: %{
                     shared: %{product_id: Ash.Changeset.get_argument(changeset, :product_id)}
                   }
                 )

                 {:ok, user}
               end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute(:last_used_product_id, :integer, public?: true)
    end
  end

  test "uses scope options" do
    scope = %Scope{actor: %{name: "Jelly Belly"}}
    assert "Hello, Jelly Belly." = User.hello_actor!(scope: scope)
    assert "Hello, Jimmy Bimmy." = User.hello_actor!(scope: scope, actor: %{name: "Jimmy Bimmy"})

    assert "Hello, William Shatner." = User.hello_actor_with_default!()
  end

  test "private arguments can be passed through code interface for create" do
    user =
      User.create_with_private!(%{first_name: "john"},
        private_arguments: %{private_tag: "test"}
      )

    assert user.last_name == "test_tagged"
  end

  test "private arguments can be passed through code interface for bulk_create" do
    %Ash.BulkResult{records: users} =
      User.create_with_private!([%{first_name: "john"}, %{first_name: "jane"}],
        private_arguments: %{private_tag: "bulk_test"},
        bulk_options: [return_records?: true]
      )

    assert length(users) == 2

    assert Enum.all?(users, fn user ->
             user.last_name == "bulk_test_tagged"
           end)
  end

  test "private arguments can be passed through code interface for update" do
    user = User.create!("john")

    updated =
      User.update_with_private!(user, %{}, private_arguments: %{private_flag: "updated"})

    assert updated.last_name == "updated_flagged"
  end

  test "private arguments can be passed through code interface for bulk_update" do
    %Ash.BulkResult{records: users} =
      User.insert!([%{first_name: "john"}, %{first_name: "jane"}],
        bulk_options: [return_records?: true]
      )

    %Ash.BulkResult{records: updated_users} =
      User.update_with_private!(users, %{},
        private_arguments: %{private_flag: "bulk_updated"},
        bulk_options: [return_records?: true]
      )

    assert length(updated_users) == 2

    assert Enum.all?(updated_users, fn user ->
             user.last_name == "bulk_updated_flagged"
           end)
  end

  test "private arguments can be passed through code interface for atomic update" do
    user = User.create!("john")

    updated =
      User.atomic_update_with_private!(user, %{}, private_arguments: %{private_flag: "updated"})

    assert updated.last_name == "updated_flagged"
  end

  test "private arguments can be passed through code interface for atomic bulk_update" do
    %Ash.BulkResult{records: users} =
      User.insert!([%{first_name: "john"}, %{first_name: "jane"}],
        bulk_options: [return_records?: true]
      )

    %Ash.BulkResult{records: updated_users} =
      User.atomic_update_with_private!(users, %{},
        private_arguments: %{private_flag: "bulk_updated"},
        bulk_options: [return_records?: true]
      )

    assert length(updated_users) == 2

    assert Enum.all?(updated_users, fn user ->
             user.last_name == "bulk_updated_flagged"
           end)
  end

  test "private arguments can be passed through code interface for destroy" do
    user = User.create!("john")

    :ok = User.destroy_with_private!(user, private_arguments: %{private_reason: "admin"})

    assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}} =
             User.get_user(user.id)
  end

  test "private arguments can be passed through code interface for bulk destroy" do
    %Ash.BulkResult{records: users} =
      User.insert!([%{first_name: "john"}, %{first_name: "jane"}],
        bulk_options: [return_records?: true]
      )

    %Ash.BulkResult{status: :success} =
      User.destroy_with_private!(users, %{},
        private_arguments: %{private_reason: "cleanup"},
        bulk_options: [return_records?: false]
      )

    Enum.each(users, fn user ->
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}} =
               User.get_user(user.id)
    end)
  end

  test "private arguments can be passed through code interface for soft destroy" do
    user = User.create!("john")

    :ok = User.soft_destroy_with_private!(user, private_arguments: %{private_reason: "admin"})

    updated_user = User.get_user!(user.id)

    assert updated_user.last_name == "soft_destroyed_by_admin"
    assert updated_user.deleted_at != nil
  end

  test "private arguments can be passed through code interface for bulk soft destroy" do
    %Ash.BulkResult{records: users} =
      User.insert!([%{first_name: "john"}, %{first_name: "jane"}],
        bulk_options: [return_records?: true]
      )

    %Ash.BulkResult{status: :success} =
      User.soft_destroy_with_private!(users, %{},
        private_arguments: %{private_reason: "cleanup"},
        bulk_options: [return_records?: false]
      )

    update_users = User.read_users!()

    Enum.each(update_users, fn updated_user ->
      assert updated_user.last_name == "soft_destroyed_by_cleanup"
      assert updated_user.deleted_at != nil
    end)
  end

  test "scope and context are merged correctly" do
    org_id = "8eaafd6e-ed0f-44cc-9429-c30c77d606dd"

    user = Ash.create!(ContextUser, %{last_used_product_id: 0})
    assert user.last_used_product_id == 0

    scope = %ContextScope{current_user: user, org_id: org_id, product_id: nil}

    user = ContextUser.enable_product!(user, 123, scope: scope)
    assert user.last_used_product_id == 123
    log_event = Ash.read_one!(LogEvent)
    user_id = user.id

    assert %Ash.Test.CodeInterfaceTest.LogEvent{
             user_id: ^user_id,
             org_id: ^org_id,
             product_id: 123,
             event: "product enabled"
           } = log_event
  end
end
