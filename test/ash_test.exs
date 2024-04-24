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
    test "with a {resource, args} as first argument" do
      assert {:ok, %User{name: "John"}} = Ash.create({User, name: "John"})
      assert {:ok, %User{name: "John"}} = Ash.create({User, %{name: "John"}})
    end
  end

  describe "create/2" do
    test "with a {record, args} as first argument and explicit action" do
      assert {:ok, %User{name: "John"}} =
               Ash.create({User, name: "John"}, action: :create)

      assert {:ok, %User{name: "John"}} =
               Ash.create({User, %{name: "John"}}, action: :create)

      assert {:ok, %User{name: "John", state: :awake}} =
               Ash.create({User, name: "John"}, action: :create_awake)

      assert {:ok, %User{name: "John", state: :awake}} =
               Ash.create({User, %{name: "John"}}, action: :create_awake)
    end
  end

  describe "create!/1" do
    test "with a {resource, args} as first argument" do
      assert %User{name: "John"} = Ash.create!({User, name: "John"})
      assert %User{name: "John"} = Ash.create!({User, %{name: "John"}})
    end
  end

  describe "create!/2" do
    test "with a {record, args} as first argument and explicit action" do
      assert %User{name: "John"} =
               Ash.create!({User, name: "John"}, action: :create)

      assert %User{name: "John"} =
               Ash.create!({User, %{name: "John"}}, action: :create)

      assert %User{name: "John", state: :awake} =
               Ash.create!({User, name: "John"}, action: :create_awake)

      assert %User{name: "John", state: :awake} =
               Ash.create!({User, %{name: "John"}}, action: :create_awake)
    end
  end

  describe "update/1" do
    test "with a {record, args} as first argument" do
      user = Ash.create!({User, name: "John"})

      assert {:ok, %User{name: "Jane"}} = Ash.update({user, name: "Jane"})
      assert {:ok, %User{name: "Jane"}} = Ash.update({user, %{name: "Jane"}})
    end
  end

  describe "update/2" do
    test "with a {record, args} as first argument and explicit action" do
      user = Ash.create!({User, name: "John"})

      assert {:ok, %User{name: "Jane"}} =
               Ash.update({user, name: "Jane"}, action: :update)

      assert {:ok, %User{name: "Jane"}} =
               Ash.update({user, %{name: "Jane"}}, action: :update)

      assert {:ok, %User{name: "John", state: :awake}} =
               Ash.update({user, state: :awake}, action: :update_state)

      assert {:ok, %User{name: "John", state: :awake}} =
               Ash.update({user, %{state: :awake}}, action: :update_state)
    end
  end

  describe "update!/1" do
    test "with a {record, args} as first argument" do
      user = Ash.create!({User, name: "John"})

      assert %User{name: "Jane"} = Ash.update!({user, name: "Jane"})
      assert %User{name: "Jane"} = Ash.update!({user, %{name: "Jane"}})
    end
  end

  describe "update!/2" do
    test "with a {record, args} as first argument and explicit action" do
      user = Ash.create!({User, name: "John"})

      assert %User{name: "Jane"} =
               Ash.update!({user, name: "Jane"}, action: :update)

      assert %User{name: "Jane"} =
               Ash.update!({user, %{name: "Jane"}}, action: :update)

      assert %User{name: "John", state: :awake} =
               Ash.update!({user, state: :awake}, action: :update_state)

      assert %User{name: "John", state: :awake} =
               Ash.update!({user, %{state: :awake}}, action: :update_state)
    end
  end
end
