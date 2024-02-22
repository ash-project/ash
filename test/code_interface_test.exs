defmodule Ash.Test.CodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.AnyApi, as: Api

  defmodule User do
    @moduledoc false
    use Ash.Resource, api: Api, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    code_interface do
      define :get_user, action: :read, get?: true
      define :get_user_safely, action: :read, get?: true, not_found_error?: false
      define :read_users, action: :read
      define :get_by_id, action: :read, get_by: [:id]
      define :create, args: [{:optional, :first_name}]
      define :hello, args: [:name]

      define :update, action: :update

      define_calculation(:full_name, args: [:first_name, :last_name])

      define_calculation(:full_name_opt,
        calculation: :full_name,
        args: [:first_name, :last_name, {:optional, :separator}]
      )

      define_calculation(:full_name_record, calculation: :full_name, args: [:_record])
    end

    actions do
      read :read do
        primary? true
      end

      create :create

      update :update

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
        argument :separator, :string, default: " ", allow_nil?: false
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :first_name, :string do
        default "fred"
      end

      attribute :last_name, :string
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
      assert %Ash.Query{action: %{name: :read}} = User.query_to_get_by_id("some uuid")
    end

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_read_users(nil)
      assert {:ok, true} == User.can_get_by_id(nil, "some uuid")
      assert User.can_read_users?(nil)
      assert User.can_get_by_id?(nil, "some uuid")
    end

    test "code interface-generated functions should check the type of their first argument and return an expressive error" do
      assert_raise ArgumentError,
                   ~r/^Initial must be a changeset with the action type of.+/i,
                   fn ->
                     User.update([], %{first_name: "Zack3", last_name: "Daniel3"})
                   end
    end
  end

  describe "read get actions" do
    test "raise on not found by default" do
      assert_raise Ash.Error.Query.NotFound, fn ->
        User.get_user!(Ash.UUID.generate())
      end
    end

    test "do not raise on not found if were defined with `not_found_error?: false`" do
      assert nil == User.get_user_safely!(Ash.UUID.generate())
    end

    test "do not raise on not found if option `not_found_error?: false` is passed" do
      assert nil == User.get_user!(Ash.UUID.generate(), not_found_error?: false)
      assert nil == User.get_user_safely!(Ash.UUID.generate(), not_found_error?: false)
    end

    test "raise on not found if option `not_found_error?: true` is passed" do
      assert_raise Ash.Error.Query.NotFound, fn ->
        User.get_user_safely!(Ash.UUID.generate(), not_found_error?: true)
      end
    end
  end

  describe "create actions" do
    test "have a helper methods to produce changeset" do
      assert %Ash.Changeset{action: %{name: :create}, attributes: %{first_name: "fred"}} =
               User.changeset_to_create()

      assert %Ash.Changeset{action: %{name: :create}, attributes: %{first_name: "bob"}} =
               User.changeset_to_create("bob")
    end

    test "have a helper to test authorization" do
      assert {:ok, true} == User.can_create(nil)
      assert {:ok, true} == User.can_create(nil, "bob")
      assert User.can_create?(nil)
      assert User.can_create?(nil, "bob")
    end
  end

  describe "calculations" do
    test "calculation value can be fetched dynamically" do
      assert {:ok, "Zach Daniel"} =
               Api.calculate(User, :full_name, refs: %{first_name: "Zach", last_name: "Daniel"})
    end

    test "the same calculation can be fetched with the calculation interface" do
      assert "Zach Daniel" = User.full_name!("Zach", "Daniel")
    end

    test "the same calculation can be fetched with the calculation interface with optional" do
      assert "Zach Daniel" = User.full_name_opt!("Zach", "Daniel")
      assert "Zach-Daniel" = User.full_name_opt!("Zach", "Daniel", "-")
    end

    test "the calculation can accept a record" do
      user =
        User
        |> Ash.Changeset.for_create(:create, %{first_name: "Zach", last_name: "Daniel"})
        |> Api.create!()

      assert "Zach Daniel" = User.full_name_record!(user)
    end
  end

  test "get_by! adds the proper arguments and filters" do
    user =
      User
      |> Ash.Changeset.for_create(:create, %{first_name: "ted", last_name: "Danson"})
      |> Api.create!()

    assert User.get_by_id!(user.id).id == user.id
  end

  test "optional arguments are optional" do
    assert User.create!().first_name == "fred"
    assert User.create!("joe").first_name == "joe"
  end
end
