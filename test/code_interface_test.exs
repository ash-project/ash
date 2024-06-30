defmodule Ash.Test.CodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(Application.get_env(__MODULE__, :notifier_test_pid), {:notification, notification})
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
      define :create, args: [{:optional, :first_name}]
      define :hello, args: [:name]
      define :create_with_map, args: [:map]
      define :update_with_map, args: [:map]

      define :bulk_create, action: :create
      define :update, action: :update
      define :destroy, action: :destroy

      define_calculation(:full_name, args: [:first_name, :last_name])

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

      action :hello, :string do
        argument :name, :string, allow_nil?: false

        run(fn input, _ ->
          {:ok, "Hello #{input.arguments.name}"}
        end)
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
        default "fred"
      end

      attribute :last_name, :string do
        public?(true)
      end
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource User do
        define :get_user, action: :read, get_by: :id

        define_calculation(:full_name, args: [:first_name, :last_name])
      end
    end
  end

  @context %{test: "value"}

  setup do
    Application.put_env(Notifier, :notifier_test_pid, self())

    :ok
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

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_read_users(nil)
      assert {:ok, true} == User.can_get_by_id(nil, "some uuid")
      assert User.can_read_users?(nil)
      assert User.can_get_by_id?(nil, "some uuid")
    end
  end

  describe "read get actions" do
    test "raise on not found by default" do
      assert_raise Ash.Error.Query.NotFound, fn ->
        User.get_user!(Ash.UUID.generate())
      end

      assert_raise Ash.Error.Query.NotFound, fn ->
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
      assert_raise Ash.Error.Query.NotFound, fn ->
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
      assert %Ash.BulkResult{status: :success} = User.bulk_create!([])
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
  end

  test "optional arguments are optional" do
    assert User.create!().first_name == "fred"
    assert User.create!("joe").first_name == "joe"
  end

  test "it handles keyword inputs properly" do
    assert User.create!("fred", [last_name: "weasley"], actor: nil)
  end
end
