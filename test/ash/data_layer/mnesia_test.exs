# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.MnesiaTest do
  use ExUnit.Case, async: false

  alias Ash.DataLayer.Mnesia, as: MnesiaDataLayer
  alias Ash.Test.Domain, as: Domain

  @default_test_table :test_users

  setup_all do
    # Start Mnesia for the entire test suite
    :mnesia.create_schema([node()])
    :mnesia.start()
    :ok
  end

  @no_pkey_test_table :test_events
  @tenant_test_table :test_tenant_tickets
  @lock_test_tables [:test_lock_posts, :test_lock_notes]

  setup do
    # Clean up any existing tables before each test
    for table <- [
          @default_test_table,
          @no_pkey_test_table,
          @tenant_test_table | @lock_test_tables
        ] do
      try do
        :mnesia.delete_table(table)
      catch
        :exit, {:aborted, {:no_exists, _}} -> :ok
      end
    end

    # Create fresh tables for each test
    :mnesia.create_table(@default_test_table, attributes: [:id, :val])
    :mnesia.create_table(@no_pkey_test_table, attributes: [:id, :val])
    :mnesia.create_table(@tenant_test_table, attributes: [:id, :val])

    for table <- @lock_test_tables do
      :mnesia.create_table(table, attributes: [:id, :val])
    end

    :persistent_term.put({__MODULE__, :runs}, :counters.new(1, []))
    :ok
  end

  defmodule MnesiaTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_users
    end

    actions do
      defaults [:read, :destroy, create: :*]

      update :update do
        accept :*
        require_atomic? false
      end
    end

    identities do
      identity :unique_name, [:name], pre_check_with: Domain
    end

    attributes do
      uuid_primary_key :id, writable?: true
      attribute :name, :string, public?: true
      attribute :age, :integer, public?: true
      attribute :title, :string, public?: true
      attribute :roles, {:array, :atom}, public?: true
      create_timestamp :inserted_at
      update_timestamp :updated_at, writable?: true, public?: true
    end
  end

  defmodule MnesiaTenantTicket do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_tenant_tickets
    end

    multitenancy do
      strategy :attribute
      attribute :org_id
    end

    actions do
      defaults [:read, :destroy, create: [:subject], update: [:subject]]

      create :save do
        accept [:id, :subject]
        upsert? true
        upsert_fields [:subject]
      end
    end

    attributes do
      uuid_primary_key :id, writable?: true
      attribute :org_id, :string, allow_nil?: false, public?: true
      attribute :subject, :string, public?: true
    end
  end

  defmodule CountRuns do
    @moduledoc false
    use Ash.Resource.Change

    @impl true
    def change(changeset, _opts, _context) do
      Ash.Changeset.before_action(changeset, fn changeset ->
        :counters.add(:persistent_term.get({Ash.DataLayer.MnesiaTest, :runs}), 1, 1)
        changeset
      end)
    end
  end

  defmodule MnesiaLockTestNote do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_lock_notes
    end

    actions do
      defaults [:read, create: :*]

      create :create_touching_posts do
        accept [:body]
        touches_resources [Ash.DataLayer.MnesiaTest.MnesiaLockTestPost]
        change CountRuns
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
    end
  end

  defmodule MnesiaLockTestPost do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_lock_posts
    end

    actions do
      defaults [:read]

      create :create do
        primary? true
        accept [:title]
        change CountRuns
      end

      create :create_with_note do
        accept [:title]
        change CountRuns

        change after_action(fn _changeset, post, _context ->
                 Ash.create!(MnesiaLockTestNote, %{body: post.title})
                 {:ok, post}
               end)
      end

      create :create_with_declared_note do
        accept [:title]
        touches_resources [MnesiaLockTestNote]
        change CountRuns

        change after_action(fn _changeset, post, _context ->
                 Ash.create!(MnesiaLockTestNote, %{body: post.title})
                 {:ok, post}
               end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    identities do
      identity :unique_title, [:title], pre_check_with: Domain
    end
  end

  defmodule MnesiaNoPkEvent do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    mnesia do
      table :test_events
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      attribute :name, :string, public?: true
    end
  end

  describe "create/2" do
    test "it creates a user" do
      resource = MnesiaTestUser
      user = %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]}

      assert {:ok, created_user} =
               MnesiaDataLayer.create(resource, Ash.Changeset.for_create(resource, :create, user))

      assert %MnesiaTestUser{
               id: _,
               name: "John",
               age: 30,
               title: "Developer",
               roles: [:admin, :user]
             } = created_user
    end

    test "it rejects a create whose primary key already exists" do
      id = Ash.UUID.generate()

      assert {:ok, _} =
               MnesiaTestUser
               |> Ash.Changeset.for_create(:create, %{id: id, name: "John"})
               |> Ash.create()

      assert {:error, %Ash.Error.Invalid{} = error} =
               MnesiaTestUser
               |> Ash.Changeset.for_create(:create, %{id: id, name: "Jane"})
               |> Ash.create()

      assert Exception.message(error) =~ "has already been taken"

      assert {:ok, %MnesiaTestUser{name: "John"}} = Ash.get(MnesiaTestUser, id)
    end

    test "it allows duplicate rows for a resource without a primary key" do
      for name <- ["a", "b", "a"] do
        assert {:ok, _} =
                 MnesiaNoPkEvent
                 |> Ash.Changeset.for_create(:create, %{name: name})
                 |> Ash.create()
      end

      names = MnesiaNoPkEvent |> Ash.read!() |> Enum.map(& &1.name) |> Enum.sort()
      assert names == ["a", "a", "b"]
    end
  end

  describe "upsert" do
    test "upsert with empty upsert_fields does not update update_timestamp" do
      past = DateTime.add(DateTime.utc_now(), -60, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      updated =
        MnesiaTestUser
        |> Ash.Changeset.for_create(:create, %{name: "Johnny", id: user.id})
        |> Ash.create!(upsert?: true, upsert_fields: [])

      assert updated.name == "John"
      assert DateTime.compare(updated.updated_at, past) == :eq
    end

    test "upsert does not update update_timestamp when touch_update_defaults? is false" do
      past = DateTime.add(DateTime.utc_now(), -60, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      updated =
        MnesiaTestUser
        |> Ash.Changeset.for_create(:create, %{name: "Johnny", id: user.id})
        |> Ash.create!(upsert?: true, touch_update_defaults?: false)

      assert updated.name == "Johnny"
      assert DateTime.compare(updated.updated_at, past) == :eq
    end

    test "upsert preserves explicitly set update_default fields when touch_update_defaults? is false" do
      past = DateTime.add(DateTime.utc_now(), -120, :second)
      explicit_time = DateTime.add(DateTime.utc_now(), -30, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      updated =
        MnesiaTestUser
        |> Ash.Changeset.for_create(:create, %{
          name: "Johnny",
          id: user.id,
          updated_at: explicit_time
        })
        |> Ash.create!(upsert?: true, touch_update_defaults?: false)

      assert updated.name == "Johnny"
      assert DateTime.compare(updated.updated_at, explicit_time) == :eq
    end

    test "an upsert on the primary key doesn't update or return a record in another tenant" do
      victim = Ash.create!(MnesiaTenantTicket, %{subject: "theirs"}, tenant: "org2")

      for opts <- [[], [return_skipped_upsert?: true]] do
        assert {:error, %{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
                 Ash.create(
                   MnesiaTenantTicket,
                   %{id: victim.id, subject: "hello"},
                   [action: :save, tenant: "org1"] ++ opts
                 )
      end

      assert %{org_id: "org2", subject: "theirs"} =
               Ash.get!(MnesiaTenantTicket, victim.id, tenant: "org2")

      assert %{org_id: "org2", subject: "hello"} =
               Ash.create!(MnesiaTenantTicket, %{id: victim.id, subject: "hello"},
                 action: :save,
                 tenant: "org2"
               )
    end
  end

  describe "changeset filters" do
    test "an update or destroy whose filter the record doesn't match is stale" do
      ticket = Ash.create!(MnesiaTenantTicket, %{subject: "mine"}, tenant: "org1")

      assert {:error, %{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
               ticket
               |> Ash.Changeset.for_update(:update, %{subject: "changed"})
               |> Ash.Changeset.filter(subject: "other")
               |> Ash.update()

      assert {:error, %{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
               ticket
               |> Ash.Changeset.for_destroy(:destroy)
               |> Ash.Changeset.filter(subject: "other")
               |> Ash.destroy()

      assert %{subject: "changed"} =
               ticket
               |> Ash.Changeset.for_update(:update, %{subject: "changed"})
               |> Ash.Changeset.filter(subject: "mine")
               |> Ash.update!()

      assert :ok =
               ticket
               |> Ash.Changeset.for_destroy(:destroy)
               |> Ash.Changeset.filter(subject: "changed")
               |> Ash.destroy()
    end
  end

  describe "aggregates" do
    test "count and exists return their values" do
      assert Ash.count!(MnesiaTestUser) == 0
      refute Ash.exists?(MnesiaTestUser)

      Ash.create!(MnesiaTestUser, %{name: "John"})

      assert Ash.count!(MnesiaTestUser) == 1
      assert Ash.exists?(MnesiaTestUser)
    end
  end

  describe "bulk_create/3" do
    test "can? bulk_create" do
      assert MnesiaDataLayer.can?(:test_resource, :bulk_create)
    end

    test "bulk_create/3 without returning records" do
      resp =
        [
          %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create)

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: nil,
               notifications: nil,
               error_count: 0
             } = resp
    end

    test "bulk_create/3 WITH returning records" do
      resp =
        [
          %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create, return_records?: true)

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: [
                 %MnesiaTestUser{
                   name: "John",
                   age: 30,
                   title: "Developer",
                   roles: [:admin, :user]
                 },
                 %MnesiaTestUser{name: "Jane", age: 25, title: "Designer", roles: [:user]},
                 %MnesiaTestUser{
                   name: "Bob",
                   age: 35,
                   title: "Manager",
                   roles: [:admin, :manager]
                 }
               ],
               notifications: nil,
               error_count: 0
             } = resp
    end

    test "bulk_create with upsert updates update_timestamp" do
      past = DateTime.add(DateTime.utc_now(), -60, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      assert DateTime.compare(user.updated_at, past) == :eq

      resp =
        [%{name: "Johnny", id: user.id}]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [:name],
          return_records?: true
        )

      assert %Ash.BulkResult{status: :success, records: [%MnesiaTestUser{} = updated]} = resp
      assert updated.name == "Johnny"
      assert DateTime.after?(updated.updated_at, past)
    end

    test "bulk_create with empty upsert does not update update_timestamp" do
      past = DateTime.add(DateTime.utc_now(), -60, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      resp =
        [%{name: "Johnny", id: user.id}]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [],
          return_records?: true
        )

      assert %Ash.BulkResult{status: :success, records: [%MnesiaTestUser{} = updated]} = resp
      assert DateTime.compare(updated.updated_at, past) == :eq
    end

    test "bulk_create with upsert does not update update_timestamp when touch_update_defaults? is false" do
      past = DateTime.add(DateTime.utc_now(), -60, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      assert DateTime.compare(user.updated_at, past) == :eq

      resp =
        [%{name: "Johnny", id: user.id}]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [:name],
          touch_update_defaults?: false,
          return_records?: true
        )

      assert %Ash.BulkResult{status: :success, records: [%MnesiaTestUser{} = updated]} = resp
      assert updated.name == "Johnny"
      assert DateTime.compare(updated.updated_at, past) == :eq
    end

    test "bulk_create with upsert preserves explicitly set update_default fields when touch_update_defaults? is false" do
      past = DateTime.add(DateTime.utc_now(), -120, :second)
      explicit_time = DateTime.add(DateTime.utc_now(), -30, :second)

      user =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user], updated_at: past}
        |> then(&Ash.create!(MnesiaTestUser, &1))

      resp =
        [%{name: "Johnny", id: user.id, updated_at: explicit_time}]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [:name, :updated_at],
          touch_update_defaults?: false,
          return_records?: true
        )

      assert %Ash.BulkResult{status: :success, records: [%MnesiaTestUser{} = updated]} = resp
      assert updated.name == "Johnny"
      assert DateTime.compare(updated.updated_at, explicit_time) == :eq
    end

    test "bulk_create/3 with UPSERT without returning records" do
      user_a =
        %{name: "John", age: 30, title: "Developer", roles: [:admin, :user]}
        |> then(&Ash.create!(MnesiaTestUser, &1))
        |> resource_to_map()

      resp =
        [
          %{user_a | name: "Johnny", age: 31},
          %{name: "Jane", age: 25, title: "Designer", roles: [:user]},
          %{name: "Bob", age: 35, title: "Manager", roles: [:admin, :manager]}
        ]
        |> Ash.bulk_create(MnesiaTestUser, :create,
          upsert?: true,
          upsert_fields: [:name],
          return_records?: true
        )

      assert %Ash.BulkResult{
               status: :success,
               errors: [],
               records: [
                 %MnesiaTestUser{
                   name: "Johnny",
                   age: 30,
                   title: "Developer",
                   roles: [:admin, :user]
                 },
                 %MnesiaTestUser{name: "Jane", age: 25, title: "Designer", roles: [:user]},
                 %MnesiaTestUser{
                   name: "Bob",
                   age: 35,
                   title: "Manager",
                   roles: [:admin, :manager]
                 }
               ],
               notifications: nil,
               error_count: 0
             } = resp
    end
  end

  describe "transactions under contention" do
    test "simultaneous creates that check an identity all succeed" do
      results = all_at_once(20, &Ash.create(MnesiaTestUser, %{name: "user #{&1}"}))

      assert Enum.all?(results, &match?({:ok, _}, &1))
      assert length(Ash.read!(MnesiaTestUser)) == 20
    end

    test "a create waiting on a lock runs its hooks once" do
      results = all_at_once(20, &Ash.create(MnesiaLockTestPost, %{title: "post #{&1}"}))

      assert Enum.all?(results, &match?({:ok, _}, &1))
      assert runs() == 20
    end

    test "an action waits for a lock on a resource it declares in touches_resources" do
      holder = hold_table_lock(:test_lock_notes)
      restarts = :mnesia.system_info(:transaction_restarts)

      task =
        Task.async(fn ->
          Ash.create(MnesiaLockTestPost, %{title: "post"}, action: :create_with_declared_note)
        end)

      assert await_restart(restarts) == :ok
      release(holder)

      assert {:ok, _} = Task.await(task)
      assert runs() == 1
      assert [_] = Ash.read!(MnesiaLockTestNote)
    end

    test "actions that lock each other's tables in opposite orders all complete, running once" do
      results =
        for _round <- 1..10 do
          all_at_once(2, fn
            1 ->
              Ash.create(MnesiaLockTestPost, %{title: "post #{System.unique_integer()}"},
                action: :create_with_declared_note
              )

            2 ->
              Ash.create(MnesiaLockTestNote, %{body: "note"}, action: :create_touching_posts)
          end)
        end

      assert Enum.all?(List.flatten(results), &match?({:ok, _}, &1))
      assert runs() == 20
    end

    test "an action that meets a lock on a resource it does not declare fails without running again" do
      holder = hold_table_lock(:test_lock_notes)

      result = Ash.create(MnesiaLockTestPost, %{title: "post"}, action: :create_with_note)
      release(holder)

      assert {:error, _} = result
      assert runs() == 1
      assert [] = Ash.read!(MnesiaLockTestPost)
    end
  end

  defp all_at_once(count, fun) do
    tasks =
      for n <- 1..count do
        Task.async(fn ->
          receive do
            :go -> fun.(n)
          end
        end)
      end

    Enum.each(tasks, &send(&1.pid, :go))
    Task.await_many(tasks)
  end

  defp runs, do: :counters.get(:persistent_term.get({__MODULE__, :runs}), 1)

  defp hold_table_lock(table) do
    test = self()

    holder =
      spawn_link(fn ->
        :mnesia.transaction(fn ->
          :mnesia.lock({:table, table}, :write)
          send(test, :locked)

          receive do
            :release -> :ok
          end
        end)

        send(test, :released)
      end)

    assert_receive :locked
    holder
  end

  defp release(holder) do
    send(holder, :release)
    assert_receive :released
  end

  defp await_restart(restarts, polls \\ 100) do
    cond do
      :mnesia.system_info(:transaction_restarts) > restarts ->
        :ok

      polls == 0 ->
        :no_restart

      true ->
        Process.sleep(10)
        await_restart(restarts, polls - 1)
    end
  end

  defp resource_to_map(resource) do
    Ash.Resource.Info.public_attributes(resource)
    |> Enum.map(& &1.name)
    |> Map.new(fn name -> {name, Map.get(resource, name)} end)
  end
end
