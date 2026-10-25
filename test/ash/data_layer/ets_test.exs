# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.DataLayer.EtsTest do
  use ExUnit.Case, async: false

  alias Ash.DataLayer.Ets, as: EtsDataLayer
  alias Ash.DataLayer.Ets.Query
  import Ash.Test

  require Ash.Query

  setup do
    on_exit(fn ->
      Ash.DataLayer.Ets.stop(__MODULE__.EtsTestUser)
    end)
  end

  alias Ash.Test.Domain, as: Domain

  defmodule EtsTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
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

  defmodule EtsNoPrimaryKeyTestEvent do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      attribute :name, :string, public?: true
    end
  end

  defmodule EtsIntegerPrimaryKeyTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, :destroy, create: :*]
    end

    attributes do
      integer_primary_key :id, writable?: true
      attribute :name, :string, public?: true
    end
  end

  defmodule EtsPosition do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    import Ash.Expr

    ets do
      private? true
    end

    actions do
      defaults [:read]

      create :create, accept: [:unrealized_pnl]

      update :add_pnl do
        change atomic_update(
                 :unrealized_pnl,
                 expr(unrealized_pnl + ^Decimal.new("0.0816"))
               )
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :unrealized_pnl, :decimal, allow_nil?: false, public?: true
    end
  end

  test "won't compile with identities that don't precheck" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule Example do
          use Ash.Resource,
            domain: Domain,
            data_layer: Ash.DataLayer.Ets

          attributes do
            uuid_primary_key :id
            attribute :name, :string, public?: true
          end

          identities do
            identity :unique_name, [:name]
          end
        end
      end)

    assert String.contains?(output, "pre_check_with")
  end

  test "debug logging formats structs without Enumerable in atomics (e.g. Decimal)" do
    previous_level = Logger.level()
    Logger.configure(level: :debug)
    on_exit(fn -> Logger.configure(level: previous_level) end)

    position =
      EtsPosition
      |> Ash.Changeset.for_create(:create, %{unrealized_pnl: Decimal.new("1.0")})
      |> Ash.create!()

    log =
      ExUnit.CaptureLog.capture_log(fn ->
        position
        |> Ash.Changeset.for_update(:add_pnl, %{})
        |> Ash.update!()
      end)

    refute log =~ "Failed to format changes"
    assert log =~ "0.0816"
  end

  test "resource_to_query" do
    assert %Query{resource: EtsTestUser} = EtsDataLayer.resource_to_query(EtsTestUser, nil)
  end

  test "limit, offset, filter, sort" do
    query = EtsDataLayer.resource_to_query(EtsTestUser, nil)
    assert {:ok, %Query{limit: 3}} = EtsDataLayer.limit(query, 3, :foo)
    assert {:ok, %Query{offset: 10}} = EtsDataLayer.offset(query, 10, :foo)
    assert {:ok, %Query{filter: :all}} = EtsDataLayer.filter(query, :all, :foo)
    assert {:ok, %Query{sort: :asc}} = EtsDataLayer.sort(query, :asc, :foo)
  end

  test "create" do
    assert %EtsTestUser{id: id, name: "Mike"} = create_user(%{name: "Mike"})

    assert [{%{id: ^id}, %EtsTestUser{name: "Mike", id: ^id}}] = user_table()
  end

  test "create rejects a duplicate primary key instead of overwriting" do
    id = Ash.UUID.generate()

    assert %EtsTestUser{id: ^id, name: "Mike"} = create_user(%{id: id, name: "Mike"})

    assert {:error, %Ash.Error.Invalid{} = error} =
             EtsTestUser
             |> Ash.Changeset.for_create(:create, %{id: id, name: "Joe"})
             |> Ash.create()

    assert Exception.message(error) =~ "has already been taken"

    assert [{%{id: ^id}, %EtsTestUser{name: "Mike", id: ^id}}] = user_table()
  end

  test "create allows duplicate rows for a resource without a primary key" do
    for name <- ["a", "b", "a"] do
      assert {:ok, _} =
               EtsNoPrimaryKeyTestEvent
               |> Ash.Changeset.for_create(:create, %{name: name})
               |> Ash.create()
    end

    names = EtsNoPrimaryKeyTestEvent |> Ash.read!() |> Enum.map(& &1.name) |> Enum.sort()
    assert names == ["a", "a", "b"]
  end

  test "update" do
    %EtsTestUser{id: id} = user = create_user(%{name: "Mike"})

    assert [{%{id: ^id}, %EtsTestUser{id: ^id, name: "Mike"}}] = user_table()

    user
    |> Ash.Changeset.for_update(:update, %{name: "Joe"})
    |> Ash.update!()

    assert [{%{id: ^id}, %EtsTestUser{name: "Joe", id: ^id}}] = user_table()
  end

  test "upsert" do
    %EtsTestUser{id: id} = create_user(%{name: "Mike"}, upsert?: true)

    assert [{%{id: ^id}, %EtsTestUser{id: ^id, name: "Mike"}}] = user_table()

    create_user(%{name: "Joe", id: id}, upsert?: true)

    assert [{%{id: ^id}, %EtsTestUser{name: "Joe", id: ^id}}] = user_table()
  end

  test "upsert with nil key value" do
    %EtsTestUser{id: id} =
      create_user(%{name: nil}, upsert?: true, upsert_identity: :unique_name)

    assert [{%{id: ^id}, %EtsTestUser{id: ^id, name: nil}}] = user_table()

    create_user(%{name: nil, age: 25},
      upsert?: true,
      upsert_identity: :unique_name
    )

    assert [{%{id: ^id}, %EtsTestUser{name: nil, age: 25}}] =
             user_table()
  end

  test "upsert with empty upsert_fields does not update update_timestamp" do
    past = DateTime.add(DateTime.utc_now(), -60, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    updated =
      create_user(%{name: "Mike Updated", id: id},
        upsert?: true,
        upsert_fields: []
      )

    assert updated.id == id
    assert updated.name == "Mike"
    assert DateTime.compare(updated.updated_at, past) == :eq
  end

  test "upsert does not update update_timestamp when touch_update_defaults? is false" do
    past = DateTime.add(DateTime.utc_now(), -60, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    updated =
      create_user(%{name: "Mike Updated", id: id},
        upsert?: true,
        touch_update_defaults?: false
      )

    assert updated.id == id
    assert updated.name == "Mike Updated"
    assert DateTime.compare(updated.updated_at, past) == :eq
  end

  test "upsert preserves explicitly set update_default fields when touch_update_defaults? is false" do
    past = DateTime.add(DateTime.utc_now(), -120, :second)
    explicit_time = DateTime.add(DateTime.utc_now(), -30, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    updated =
      create_user(%{name: "Mike Updated", id: id, updated_at: explicit_time},
        upsert?: true,
        touch_update_defaults?: false
      )

    assert updated.id == id
    assert updated.name == "Mike Updated"
    assert DateTime.compare(updated.updated_at, explicit_time) == :eq
  end

  test "bulk_create with upsert updates update_timestamp" do
    past = DateTime.add(DateTime.utc_now(), -60, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    assert [{_, %EtsTestUser{updated_at: stored}}] = user_table()
    assert DateTime.compare(stored, past) == :eq

    result =
      Ash.bulk_create!(
        [%{name: "Mike Updated", id: id}],
        EtsTestUser,
        :create,
        upsert?: true,
        upsert_fields: [:name],
        return_records?: true
      )

    assert [%EtsTestUser{id: ^id, name: "Mike Updated", updated_at: new_updated_at}] =
             result.records

    assert DateTime.after?(new_updated_at, past)
  end

  test "bulk_create with empty upsert does not update update_timestamp" do
    past = DateTime.add(DateTime.utc_now(), -60, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    result =
      Ash.bulk_create!(
        [%{name: "Mike Updated", id: id}],
        EtsTestUser,
        :create,
        upsert?: true,
        upsert_fields: [],
        return_records?: true
      )

    assert [%EtsTestUser{id: ^id, updated_at: new_updated_at}] = result.records
    assert DateTime.compare(new_updated_at, past) == :eq
  end

  test "bulk_create with upsert does not update update_timestamp when touch_update_defaults? is false" do
    past = DateTime.add(DateTime.utc_now(), -60, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    assert [{_, %EtsTestUser{updated_at: stored}}] = user_table()
    assert DateTime.compare(stored, past) == :eq

    result =
      Ash.bulk_create!(
        [%{name: "Mike Updated", id: id}],
        EtsTestUser,
        :create,
        upsert?: true,
        upsert_fields: [:name],
        touch_update_defaults?: false,
        return_records?: true
      )

    assert [%EtsTestUser{id: ^id, name: "Mike Updated", updated_at: new_updated_at}] =
             result.records

    assert DateTime.compare(new_updated_at, past) == :eq
  end

  test "bulk_create with upsert preserves explicitly set update_default fields when touch_update_defaults? is false" do
    past = DateTime.add(DateTime.utc_now(), -120, :second)
    explicit_time = DateTime.add(DateTime.utc_now(), -30, :second)

    %EtsTestUser{id: id} = create_user(%{name: "Mike", updated_at: past})

    result =
      Ash.bulk_create!(
        [%{name: "Mike Updated", id: id, updated_at: explicit_time}],
        EtsTestUser,
        :create,
        upsert?: true,
        upsert_fields: [:name, :updated_at],
        touch_update_defaults?: false,
        return_records?: true
      )

    assert [%EtsTestUser{id: ^id, name: "Mike Updated", updated_at: new_updated_at}] =
             result.records

    assert DateTime.compare(new_updated_at, explicit_time) == :eq
  end

  test "destroy" do
    mike = create_user(%{name: "Mike"})
    %EtsTestUser{id: joes_id} = joe = create_user(%{name: "Joe"})

    assert length(user_table()) == 2

    Ash.destroy!(mike)

    assert [{%{id: ^joes_id}, ^joe}] = strip_metadata(user_table())
  end

  test "get" do
    create_user(%{name: "Mike"})
    create_user(%{name: "Joe"})
    %{id: id} = create_user(%{name: "Matthew"})
    create_user(%{name: "Zachary"})

    assert %EtsTestUser{id: ^id, name: "Matthew"} = Ash.get!(EtsTestUser, id)
  end

  test "sort" do
    mike = create_user(%{name: "Mike"})
    joe = create_user(%{name: "Joe"})
    matthew = create_user(%{name: "Matthew"})
    zachary = create_user(%{name: "Zachary"})

    query =
      EtsTestUser
      |> Ash.Query.new()
      |> Ash.Query.sort(:name)

    assert [^joe, ^matthew, ^mike, ^zachary] = strip_metadata(Ash.read!(query))
  end

  test "limit" do
    _mike = create_user(%{name: "Mike"})
    joe = create_user(%{name: "Joe"})
    matthew = create_user(%{name: "Matthew"})
    _zachary = create_user(%{name: "Zachary"})

    query =
      EtsTestUser
      |> Ash.Query.new()
      |> Ash.Query.sort(:name)
      |> Ash.Query.limit(2)

    assert [^joe, ^matthew] = strip_metadata(Ash.read!(query))
  end

  test "offset" do
    mike = create_user(%{name: "Mike"})
    _joe = create_user(%{name: "Joe"})
    matthew = create_user(%{name: "Matthew"})
    zachary = create_user(%{name: "Zachary"})

    query =
      EtsTestUser
      |> Ash.Query.new()
      |> Ash.Query.sort(:name)
      |> Ash.Query.offset(1)

    assert [^matthew, ^mike, ^zachary] = strip_metadata(Ash.read!(query))
  end

  describe "filter" do
    setup do
      mike = create_user(%{name: "Mike", age: 37, title: "Dad", roles: [:user]})
      joe = create_user(%{name: "Joe", age: 11, roles: [:admin, :support]})
      matthew = create_user(%{name: "Matthew", age: 9, roles: [:admin, :manager]})
      zachary = create_user(%{name: "Zachary", age: 6, roles: [:admin, :manager]})
      %{mike: mike, zachary: zachary, matthew: matthew, joe: joe}
    end

    test "values", %{zachary: zachary, matthew: matthew, joe: joe} do
      assert [^zachary] = strip_metadata(filter_users(name: "Zachary"))
      assert [^joe] = strip_metadata(filter_users(name: "Joe"))
      assert [^matthew] = strip_metadata(filter_users(age: 9))
    end

    test "has", %{matthew: matthew, zachary: zachary, joe: joe} do
      assert [^joe, ^matthew, ^zachary] =
               strip_metadata(filter_users(roles: [has: :admin]))

      assert [] =
               filter_users(roles: [has: :unknown])
    end

    test "intersects", %{matthew: matthew, zachary: zachary, joe: joe} do
      assert [^joe, ^matthew, ^zachary] =
               strip_metadata(filter_users(roles: [intersects: [:manager, :support]]))

      assert [] =
               filter_users(roles: [intersects: [:unknown]])
    end

    test "or, in, eq", %{mike: mike, zachary: zachary, joe: joe} do
      assert [^joe, ^mike, ^zachary] =
               strip_metadata(
                 filter_users(
                   or: [
                     [name: [in: ["Zachary", "Mike"]]],
                     [age: [eq: 11]]
                   ]
                 )
               )
    end

    test "and, in, eq", %{mike: mike} do
      assert [^mike] =
               strip_metadata(
                 filter_users(
                   and: [
                     [name: [in: ["Zachary", "Mike"]]],
                     [age: [eq: 37]]
                   ]
                 )
               )
    end

    test "and, in, not", %{zachary: zachary} do
      assert [^zachary] =
               strip_metadata(
                 filter_users(
                   and: [
                     [name: [in: ["Zachary", "Mike"]]],
                     [not: [age: 37]]
                   ]
                 )
               )
    end

    test "gt", %{mike: mike, joe: joe} do
      assert [^joe, ^mike] = strip_metadata(filter_users(age: [gt: 10]))
    end

    test "lt", %{zachary: zachary, matthew: matthew} do
      assert [^matthew, ^zachary] = strip_metadata(filter_users(age: [lt: 10]))
    end

    test "boolean", %{zachary: zachary, matthew: matthew} do
      assert [^matthew, ^zachary] = strip_metadata(filter_users(and: [true, age: [lt: 10]]))
    end

    test "is_nil", %{zachary: zachary, matthew: matthew, joe: joe} do
      assert [^joe, ^matthew, ^zachary] = strip_metadata(filter_users(title: [is_nil: true]))
    end
  end

  describe "integer primary key" do
    test "create" do
      supplied_id = 1

      %EtsIntegerPrimaryKeyTestUser{id: id} =
        EtsIntegerPrimaryKeyTestUser
        |> Ash.Changeset.for_create(:create, %{id: supplied_id, name: "Mike"})
        |> Ash.create!()

      assert supplied_id == id
    end

    test "create id key not supplied" do
      assert_raise(Ash.Error.Invalid, ~r/invalid primary key/i, fn ->
        EtsIntegerPrimaryKeyTestUser
        |> Ash.Changeset.for_create(:create, %{name: "Mike"})
        |> Ash.create!()
      end)
    end

    test "bulk create" do
      supplied_ids = [1, 2]

      result =
        Ash.bulk_create!(
          [
            %{id: List.first(supplied_ids), name: "Foo"},
            %{id: List.last(supplied_ids), name: "Bar"}
          ],
          EtsIntegerPrimaryKeyTestUser,
          :create,
          return_records?: true
        )

      ids =
        result.records
        |> Enum.map(& &1.id)

      assert Enum.sort(supplied_ids) == Enum.sort(ids)
    end

    test "bulk create id key not supplied" do
      assert_raise(Ash.Error.Invalid, ~r/invalid primary key/i, fn ->
        Ash.bulk_create!([%{name: "Foo"}, %{name: "Bar"}], EtsIntegerPrimaryKeyTestUser, :create)
      end)
    end
  end

  defmodule EtsMultitenantTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    multitenancy do
      strategy :context
    end

    actions do
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end
  end

  # Tenant values are usually request-derived. Reading an unknown tenant must not
  # intern an atom, create an ETS table, or spawn a table manager, because none of
  # those are ever reclaimed. See GHSA-2x3m-xq2g-j5v2.
  describe "context multitenancy does not allocate for unknown tenants" do
    setup do
      # warm up lazy module loading so it does not pollute the measurements
      Ash.read!(EtsMultitenantTestUser, tenant: "warmup")
      on_exit(fn -> Ash.DataLayer.Ets.stop(EtsMultitenantTestUser, "warmup") end)
      :ok
    end

    test "reading an unknown tenant allocates nothing and returns no records" do
      before_atoms = :erlang.system_info(:atom_count)
      before_processes = :erlang.system_info(:process_count)
      before_tables = length(:ets.all())

      for i <- 1..50 do
        assert [] = Ash.read!(EtsMultitenantTestUser, tenant: "unknown-tenant-#{i}")
      end

      assert :erlang.system_info(:atom_count) == before_atoms
      assert :erlang.system_info(:process_count) == before_processes
      assert length(:ets.all()) == before_tables
    end

    test "writing a tenant still creates its table, and reads then see the data" do
      on_exit(fn -> Ash.DataLayer.Ets.stop(EtsMultitenantTestUser, "written") end)

      user =
        EtsMultitenantTestUser
        |> Ash.Changeset.for_create(:create, %{name: "Mike"}, tenant: "written")
        |> Ash.create!()

      assert [%{name: "Mike"}] = Ash.read!(EtsMultitenantTestUser, tenant: "written")
      assert [] = Ash.read!(EtsMultitenantTestUser, tenant: "not-written")

      user
      |> Ash.Changeset.for_update(:update, %{name: "Zach"}, tenant: "written")
      |> Ash.update!()

      assert [%{name: "Zach"}] = Ash.read!(EtsMultitenantTestUser, tenant: "written")
    end

    test "updating a record in a tenant that was never written is a stale record" do
      user = struct(EtsMultitenantTestUser, id: Ash.UUID.generate(), name: "Ghost")

      user = %{
        user
        | __meta__: %Ecto.Schema.Metadata{state: :loaded, schema: EtsMultitenantTestUser}
      }

      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Changes.StaleRecord{}]}} =
               user
               |> Ash.Changeset.for_update(:update, %{name: "Zach"}, tenant: "never-written")
               |> Ash.update()

      assert :ok =
               user
               |> Ash.Changeset.for_destroy(:destroy, %{}, tenant: "never-written")
               |> Ash.destroy()
    end
  end

  defmodule EtsSharedTableTestEvent do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end
  end

  test "processes making the first create on a shared table at once all succeed" do
    on_exit(fn -> Ash.DataLayer.Ets.stop(EtsSharedTableTestEvent) end)

    tasks =
      for i <- 1..50 do
        Task.async(fn ->
          receive do
            :go -> Ash.create(EtsSharedTableTestEvent, %{name: "event #{i}"})
          end
        end)
      end

    Enum.each(tasks, &send(&1.pid, :go))

    assert [] == tasks |> Task.await_many() |> Enum.reject(&match?({:ok, _}, &1))
    assert length(Ash.read!(EtsSharedTableTestEvent)) == 50
  end

  defp filter_users(filter) do
    EtsTestUser
    |> Ash.Query.new()
    |> Ash.Query.sort(:name)
    |> Ash.Query.filter(^filter)
    |> Ash.read!()
  end

  defp create_user(attrs, opts \\ []) do
    EtsTestUser
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!(opts)
    |> strip_metadata()
  end

  defp user_table do
    table = Process.get({:ash_ets_table, EtsTestUser, nil})

    if table do
      table
      |> ETS.Set.to_list!()
      |> Enum.map(&cast_user!/1)
    else
      []
    end
  end

  defmodule EtsTransactionAccount do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy]

      create :create do
        primary? true
        accept [:email]
      end

      create :create_then_fail do
        accept [:email]

        change after_action(fn _changeset, _record, _context ->
                 raise "after the write"
               end)
      end

      create :create_slowly do
        accept [:email]

        change after_action(fn _changeset, record, _context ->
                 Process.sleep(50)
                 {:ok, record}
               end)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :email, :string, public?: true
    end

    identities do
      identity :unique_email, [:email], pre_check_with: Domain
    end
  end

  defmodule EtsTransactionLeft do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read]

      create :create do
        primary? true
        touches_resources [Ash.DataLayer.EtsTest.EtsTransactionRight]
      end
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule EtsTransactionRight do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read]

      create :create do
        primary? true
        touches_resources [Ash.DataLayer.EtsTest.EtsTransactionLeft]
      end
    end

    attributes do
      uuid_primary_key :id
    end
  end

  defmodule EtsTransactionOrganization do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, create: [:owner_id]]
    end

    attributes do
      uuid_primary_key :id
      attribute :owner_id, :uuid, public?: true
    end
  end

  defmodule EtsTransactionPost do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:read, create: [:organization_id]]
    end

    policies do
      policy action_type(:read) do
        authorize_if always()
      end

      policy action_type(:create) do
        authorize_if expr(organization.owner_id == ^actor(:id))
      end
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :organization, Ash.DataLayer.EtsTest.EtsTransactionOrganization,
        public?: true,
        attribute_writable?: true
    end
  end

  # Ash's test config sets `ets_transactions?`.
  describe "transactions on a shared table" do
    setup do
      on_exit(fn ->
        for resource <- [
              EtsTransactionAccount,
              EtsTransactionLeft,
              EtsTransactionRight,
              EtsTransactionOrganization,
              EtsTransactionPost
            ] do
          Ash.DataLayer.Ets.stop(resource)
        end
      end)
    end

    test "only a private table has none" do
      refute Ash.DataLayer.data_layer_can?(EtsTestUser, :transact)
      assert Ash.DataLayer.data_layer_can?(EtsTransactionAccount, :transact)
    end

    test "a create's filter policy is checked after the insert, in the transaction" do
      owner = Ash.UUID.generate()
      organization = Ash.create!(EtsTransactionOrganization, %{owner_id: owner})

      assert {:ok, _} =
               Ash.create(EtsTransactionPost, %{organization_id: organization.id},
                 actor: %{id: owner}
               )

      assert {:error, %Ash.Error.Forbidden{}} =
               Ash.create(EtsTransactionPost, %{organization_id: organization.id},
                 actor: %{id: Ash.UUID.generate()}
               )

      assert [_] = Ash.read!(EtsTransactionPost)
    end

    test "an action that runs past its timeout is undone" do
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.Timeout{}]}} =
               Ash.create(EtsTransactionAccount, %{email: "late@example.com"},
                 action: :create_slowly,
                 timeout: 10
               )

      assert [] = Ash.read!(EtsTransactionAccount)
    end

    test "simultaneous creates of one identity value keep one record" do
      results =
        at_once(20, fn _ -> Ash.create(EtsTransactionAccount, %{email: "same@example.com"}) end)

      assert Enum.count(results, &match?({:ok, _}, &1)) == 1
      assert Enum.count(results, &match?({:error, %Ash.Error.Invalid{}}, &1)) == 19
      assert [_] = Ash.read!(EtsTransactionAccount)
    end

    test "a rollback undoes the writes made in the transaction" do
      assert {:error, _} =
               Ash.transaction(EtsTransactionAccount, fn ->
                 Ash.create!(EtsTransactionAccount, %{email: "kept@example.com"})
                 Ash.DataLayer.rollback(EtsTransactionAccount, :rolled_back)
               end)

      assert [] = Ash.read!(EtsTransactionAccount)
    end

    test "an action that raises after its write leaves no record" do
      result =
        try do
          Ash.create(EtsTransactionAccount, %{email: "gone@example.com"},
            action: :create_then_fail
          )
        rescue
          error -> {:raised, error}
        end

      refute match?({:ok, _}, result)
      assert [] = Ash.read!(EtsTransactionAccount)
    end

    test "actions that lock each other's tables in opposite orders all complete" do
      results =
        for _round <- 1..10 do
          at_once(2, fn
            1 -> Ash.create(EtsTransactionLeft, %{})
            2 -> Ash.create(EtsTransactionRight, %{})
          end)
        end

      assert Enum.all?(List.flatten(results), &match?({:ok, _}, &1))
    end
  end

  # Runs `fun` for 1..count in as many processes released together, giving each three
  # seconds; one still running then is reported as `:still_running`.
  defp at_once(count, fun) do
    tasks =
      for n <- 1..count do
        Task.async(fn ->
          receive do
            :go -> fun.(n)
          end
        end)
      end

    Enum.each(tasks, &send(&1.pid, :go))

    tasks
    |> Task.yield_many(3_000)
    |> Enum.map(fn
      {_task, {:ok, result}} ->
        result

      {task, nil} ->
        Task.shutdown(task, :brutal_kill)
        :still_running
    end)
  end

  defp cast_user!({pkey, attrs}) do
    {:ok, user} = Ash.DataLayer.Ets.cast_record(attrs, EtsTestUser)
    {pkey, user}
  end
end
