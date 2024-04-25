defmodule Ash.Test.AshTest do
  @moduledoc false

  use ExUnit.Case, async: true

  defmodule Domain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule User do
    @moduledoc false

    use Ash.Resource,
      domain: Domain

    attributes do
      uuid_primary_key :id

      attribute :name, :string, public?: true

      attribute :state, :atom do
        public? true
        default :sleeping
        constraints one_of: [:sleeping, :awake]
      end
    end

    actions do
      default_accept [:name, :state]
      defaults [:create, :update]

      create :create_awake do
        accept [:name]

        argument :name, :string

        change set_attribute(:name, arg(:name))
        change set_attribute(:state, :awake)
      end

      update :update_state do
        accept [:state]

        argument :state, :atom

        change set_attribute(:state, arg(:state))
      end
    end
  end

  describe "create/1" do
    test "with a resource as first argument" do
      assert {:ok, %User{name: nil}} = Ash.create(User)
    end
  end

  describe "create/2" do
    test "with a resource as first argument, with params as second argument" do
      assert {:ok, %User{name: "Alice"}} = Ash.create(User, %{name: "Alice"})
    end

    test "with a resource as first argument, with opts as second argument" do
      assert {:ok, %User{name: nil}} = Ash.create(User, action: :create)
    end
  end

  describe "create/3" do
    test "with a record as first argument, then params and opts" do
      assert {:ok, %User{name: "Alice"}} =
               Ash.create(User, %{name: "Alice"}, action: :create)

      assert {:ok, %User{name: "Alice", state: :awake}} =
               Ash.create(User, %{name: "Alice"}, action: :create_awake)
    end
  end

  describe "create!/1" do
    test "with a resource as first argument" do
      assert %User{name: nil} = Ash.create!(User)
    end
  end

  describe "create!/2" do
    test "with a resource as first argument, with params as second argument" do
      assert %User{name: "Alice"} = Ash.create!(User, %{name: "Alice"})
    end

    test "with a resource as first argument, with opts as second argument" do
      assert %User{name: nil} = Ash.create!(User, action: :create)
    end
  end

  describe "create!/3" do
    test "with a record as first argument, then params and opts" do
      assert %User{name: "Alice"} =
               Ash.create!(User, %{name: "Alice"}, action: :create)

      assert %User{name: "Alice", state: :awake} =
               Ash.create!(User, %{name: "Alice"}, action: :create_awake)
    end
  end

  describe "update/1" do
    test "with a record as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Alice"}} = Ash.update(user)
    end
  end

  describe "update/2" do
    test "with a record as first argument, with params as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Bob"}} = Ash.update(user, %{name: "Bob"})
    end

    test "with a record as first argument, with opts as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Alice"}} = Ash.update(user, action: :update)
    end
  end

  describe "update/3" do
    test "with a record as first argument, then params and opts" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Bob"}} =
               Ash.update(user, %{name: "Bob"}, action: :update)

      assert {:ok, %User{name: "Alice", state: :awake}} =
               Ash.update(user, %{state: :awake}, action: :update_state)
    end
  end

  describe "update!/1" do
    test "with a record as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Alice"} = Ash.update!(user)
    end
  end

  describe "update!/2" do
    test "with a record as first argument, with params as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Bob"} = Ash.update!(user, %{name: "Bob"})
    end

    test "with a record as first argument, with opts as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Alice"} = Ash.update!(user, action: :update)
    end
  end

  describe "update!/3" do
    test "with a record as first argument and explicit action" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Bob"} =
               Ash.update!(user, %{name: "Bob"}, action: :update)

      assert %User{name: "Alice", state: :awake} =
               Ash.update!(user, %{state: :awake}, action: :update_state)
    end
  end
end
