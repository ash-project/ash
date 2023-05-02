defmodule Ash.Test.CodeInterfaceTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    code_interface do
      define_for Ash.Test.CodeInterfaceTest.Api
      define :get_user, action: :read, get?: true, args: [:id]
      define :read_users, action: :read
      define :get_by_id, action: :read, get_by: [:id]
      define :create, args: [{:optional, :first_name}]
      define :hello, args: [:name]

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

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
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

  describe "basic actions" do
    test "basic actions can be invoked" do
      assert "Hello fred" == User.hello!("fred")
      assert {:ok, "Hello george"} == User.hello("george")
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

  test "get! raises on not found" do
    assert_raise Ash.Error.Query.NotFound, fn ->
      User.get_user!(Ash.UUID.generate())
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
