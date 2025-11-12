# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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
      defaults [:read, :create, :update]

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

      action :action do
        run fn _input, _context ->
          :ok
        end
      end
    end

    calculations do
      calculate :awaken?, :boolean, expr(^context(:awake?)), public?: true
    end
  end

  describe "create/1" do
    test "with a changeset as first argument" do
      assert {:ok, %User{name: nil}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create()
    end

    test "with a resource as first argument" do
      assert {:ok, %User{name: nil}} = Ash.create(User)
    end
  end

  describe "create/2" do
    test "with a changeset as first argument, with params as second argument" do
      assert {:ok, %User{name: "Alice"}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create(%{name: "Alice"})

      assert_raise ArgumentError, fn ->
        User
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_create(:create)
        |> Ash.create(%{name: "Alice"})
      end
    end

    test "with a changeset as first argument, with opts as second argument" do
      assert {:ok, %User{name: nil}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create(action: :create)
    end

    test "with a resource as first argument, with params as second argument" do
      assert {:ok, %User{name: "Alice"}} = Ash.create(User, %{name: "Alice"})
    end

    test "with a resource as first argument, with opts as second argument" do
      assert {:ok, %User{name: nil}} = Ash.create(User, action: :create)
    end
  end

  describe "create/3" do
    test "with a changeset as first argument, then params and opts" do
      assert {:ok, %User{name: "Alice"}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create(%{name: "Alice"})

      assert {:ok, %User{name: "Alice", state: :awake}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create(%{name: "Alice"}, action: :create_awake)

      assert_raise ArgumentError, fn ->
        User
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_create(:create)
        |> Ash.create(%{name: "Alice"})
      end
    end

    test "with a changeset as first argument, with params and opts" do
      assert {:ok, %User{name: "Alice"}} =
               User
               |> Ash.Changeset.new()
               |> Ash.create(%{name: "Alice"}, action: :create)
    end

    test "with a record as first argument, then params and opts" do
      assert {:ok, %User{name: "Alice"}} =
               Ash.create(User, %{name: "Alice"}, action: :create)

      assert {:ok, %User{name: "Alice", state: :awake}} =
               Ash.create(User, %{name: "Alice"}, action: :create_awake)
    end
  end

  describe "create!/1" do
    test "with a changeset as first argument" do
      assert %User{name: nil} =
               User
               |> Ash.Changeset.new()
               |> Ash.create!()
    end

    test "with a resource as first argument" do
      assert %User{name: nil} = Ash.create!(User)
    end
  end

  describe "create!/2" do
    test "with a changeset as first argument, with params as second argument" do
      assert %User{name: "Alice"} =
               User
               |> Ash.Changeset.new()
               |> Ash.create!(%{name: "Alice"})

      assert_raise ArgumentError, fn ->
        User
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(%{name: "Alice"})
      end
    end

    test "with a changeset as first argument, with opts as second argument" do
      assert %User{name: nil} =
               User
               |> Ash.Changeset.new()
               |> Ash.create!(action: :create)
    end

    test "with a resource as first argument, with params as second argument" do
      assert %User{name: "Alice"} = Ash.create!(User, %{name: "Alice"})
    end

    test "with a resource as first argument, with opts as second argument" do
      assert %User{name: nil} = Ash.create!(User, action: :create)
    end
  end

  describe "create!/3" do
    test "with a changeset as first argument, then params and opts" do
      assert %User{name: "Alice"} =
               User
               |> Ash.Changeset.new()
               |> Ash.create!(%{name: "Alice"})

      assert %User{name: "Alice", state: :awake} =
               User
               |> Ash.Changeset.new()
               |> Ash.create!(%{name: "Alice"}, action: :create_awake)

      assert_raise ArgumentError, fn ->
        User
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!(%{name: "Alice"})
      end
    end

    test "with a record as first argument, then params and opts" do
      assert %User{name: "Alice"} =
               Ash.create!(User, %{name: "Alice"}, action: :create)

      assert %User{name: "Alice", state: :awake} =
               Ash.create!(User, %{name: "Alice"}, action: :create_awake)
    end
  end

  describe "update/1" do
    test "with a changeset as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Alice"}} =
               user
               |> Ash.Changeset.new()
               |> Ash.update()
    end

    test "with a record as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Alice"}} = Ash.update(user)
    end
  end

  describe "update/2" do
    test "with a changeset as first argument, with params as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Bob"}} =
               user
               |> Ash.Changeset.new()
               |> Ash.update(%{name: "Bob"})

      assert_raise ArgumentError, fn ->
        user
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_update(:update)
        |> Ash.update(%{name: "Bob"})
      end
    end

    test "with a changeset as first argument, with opts as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Alice"} =
               user
               |> Ash.Changeset.new()
               |> Ash.update!(action: :update)
    end

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
    test "with a changeset as first argument, then params and opts" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Bob"}} =
               user
               |> Ash.Changeset.new()
               |> Ash.update(%{name: "Bob"}, action: :update)

      assert {:ok, %User{name: "Alice", state: :awake}} =
               user
               |> Ash.Changeset.new()
               |> Ash.update(%{state: :awake}, action: :update_state)

      assert_raise ArgumentError, fn ->
        user
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_update(:update)
        |> Ash.update(%{name: "Bob"})
      end
    end

    test "with a record as first argument, then params and opts" do
      user = Ash.create!(User, %{name: "Alice"})

      assert {:ok, %User{name: "Bob"}} =
               Ash.update(user, %{name: "Bob"}, action: :update)

      assert {:ok, %User{name: "Alice", state: :awake}} =
               Ash.update(user, %{state: :awake}, action: :update_state)
    end
  end

  describe "update!/1" do
    test "with a changeset as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Alice"} =
               user
               |> Ash.Changeset.new()
               |> Ash.update!()
    end

    test "with a record as first argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Alice"} = Ash.update!(user)
    end
  end

  describe "update!/2" do
    test "with a changeset as first argument, with params as second argument" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Bob"} =
               user
               |> Ash.Changeset.new()
               |> Ash.update!(%{name: "Bob"})

      assert_raise ArgumentError, fn ->
        user
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_update(:update)
        |> Ash.update!(%{name: "Bob"})
      end
    end

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
    test "with a changeset as first argument, then params and opts" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Bob"} =
               user
               |> Ash.Changeset.new()
               |> Ash.update!(%{name: "Bob"}, action: :update)

      assert %User{name: "Alice", state: :awake} =
               user
               |> Ash.Changeset.new()
               |> Ash.update!(%{state: :awake}, action: :update_state)

      assert_raise ArgumentError, fn ->
        user
        |> Ash.Changeset.new()
        |> Ash.Changeset.for_update(:update)
        |> Ash.update!(%{name: "Bob"})
      end
    end

    test "with a record as first argument and explicit action" do
      user = Ash.create!(User, %{name: "Alice"})

      assert %User{name: "Bob"} =
               Ash.update!(user, %{name: "Bob"}, action: :update)

      assert %User{name: "Alice", state: :awake} =
               Ash.update!(user, %{state: :awake}, action: :update_state)
    end
  end

  describe "calculate/2" do
    test "with opts" do
      user = Ash.create!(User, %{name: "Alice"})

      opts =
        %Ash.Resource.Calculation.Context{source_context: %{shared: %{awake?: true}}}
        |> Ash.Scope.to_opts()

      assert {:ok, true} = Ash.calculate(user, :awaken?, opts)
    end
  end

  describe "run_action/2" do
    test "with opts" do
      input = Ash.ActionInput.for_action(User, :action, %{})

      opts = %Ash.Resource.Change.Context{} |> Ash.Scope.to_opts()

      assert :ok = Ash.run_action(input, opts)
    end
  end

  defmodule Notifier do
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
    end
  end

  defmodule Item do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      notifiers: [Notifier]

    mnesia do
      table :ash_transactions
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string, allow_nil?: false, public?: true

      timestamps()
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, :create, :update]
    end
  end

  describe "transaction/3" do
    setup do
      import ExUnit.CaptureLog

      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [Item])
      end)

      on_exit(fn ->
        capture_log(fn ->
          :mnesia.stop()
          :mnesia.delete_schema([node()])
        end)
      end)
    end

    test "returns ok tuple with last value from the transaction" do
      result = Ash.transaction(Item, fn -> :yep end)

      assert {:ok, :yep} == result
    end

    test "collects and sends notifications after the transaction is done" do
      Ash.transaction(Item, fn ->
        Ash.create!(Item, %{title: "it"})

        refute_received {:notification, %{action: %{type: :create}, resource: Item}}
      end)

      assert_receive {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "does not send notifications if the transaction has failed" do
      result =
        Ash.transaction(Item, fn ->
          Ash.create!(Item, %{title: "it"})
          Ash.create!(Item, %{title: nil})
        end)

      assert {:error, %Ash.Error.Invalid{}} = result

      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "returns notifications instead of sending them if `return_notifications?` is true" do
      result =
        Ash.transaction(
          Item,
          fn ->
            Ash.create!(Item, %{title: "it"})
          end,
          return_notifications?: true
        )

      assert {:ok, _, [%{action: %{type: :create}, resource: Item}]} = result

      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end
  end

  describe "get!/2" do
    test "raises error when action does not exist" do
      user = Ash.create!(User, %{name: "Alice"})

      assert_raise Ash.Error.Invalid, ~r/no such action/i, fn ->
        Ash.get!(User, user.id, action: :this_action_does_not_exist)
      end
    end
  end

  describe "transact/3" do
    setup do
      import ExUnit.CaptureLog

      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [Item])
      end)

      on_exit(fn ->
        capture_log(fn ->
          :mnesia.stop()
          :mnesia.delete_schema([node()])
        end)
      end)
    end

    test "returns ok tuple with last value from the transaction" do
      result = Ash.transact(Item, fn -> :yep end)

      assert {:ok, :yep} == result
    end

    test "collects and sends notifications after the transaction is done" do
      Ash.transact(Item, fn ->
        Ash.create!(Item, %{title: "it"})

        refute_received {:notification, %{action: %{type: :create}, resource: Item}}
      end)

      assert_receive {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "does not send notifications if the transaction has failed" do
      result =
        Ash.transact(Item, fn ->
          Ash.create!(Item, %{title: "it"})
          Ash.create!(Item, %{title: nil})
        end)

      assert {:error, %Ash.Error.Invalid{}} = result

      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "returns notifications instead of sending them if `return_notifications?` is true" do
      result =
        Ash.transact(
          Item,
          fn ->
            Ash.create!(Item, %{title: "it"})
          end,
          return_notifications?: true
        )

      assert {:ok, _, [%{action: %{type: :create}, resource: Item}]} = result

      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "automatically rolls back when callback returns {:error, _}" do
      Ash.transact(Item, fn ->
        Ash.create!(Item, %{title: "valid"})
        {:error, :something_went_wrong}
      end)

      assert {:ok, []} = Ash.read(Item)

      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end

    test "can also use explicit Ash.DataLayer.rollback" do
      Ash.transact(Item, fn ->
        Ash.create!(Item, %{title: "valid"})
        Ash.DataLayer.rollback(Item, :something_went_wrong)
      end)

      assert {:ok, []} = Ash.read(Item)
      refute_received {:notification, %{action: %{type: :create}, resource: Item}}
    end
  end
end
