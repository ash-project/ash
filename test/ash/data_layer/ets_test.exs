# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
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
    end
  end

  defmodule EtsIntegerPrimaryKeyTestUser do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read, :destroy, create: :*]
    end

    attributes do
      integer_primary_key :id, writable?: true
      attribute :name, :string, public?: true
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

  defp cast_user!({pkey, attrs}) do
    {:ok, user} = Ash.DataLayer.Ets.cast_record(attrs, EtsTestUser)
    {pkey, user}
  end
end
