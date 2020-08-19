defmodule Ash.DataLayer.EtsTest do
  use ExUnit.Case, async: false

  alias Ash.DataLayer.Ets, as: EtsDataLayer
  alias Ash.DataLayer.Ets.Query
  alias Ash.Filter.Predicate.{Eq, GreaterThan, In, LessThan}

  setup do
    on_exit(fn ->
      case ETS.Set.wrap_existing(EtsTestUser) do
        {:error, :table_not_found} -> :ok
        {:ok, set} -> ETS.Set.delete_all!(set)
      end
    end)
  end

  defmodule EtsTestUser do
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
      destroy(:default)
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
      attribute :age, :integer
    end
  end

  defmodule EtsApiTest do
    use Ash.Api

    resources do
      resource EtsTestUser
    end
  end

  test "can?" do
    assert EtsDataLayer.can?(EtsTestUser, :async_engine) == false
    assert EtsDataLayer.can?(EtsTestUser, :composite_primary_key) == true
    assert EtsDataLayer.can?(EtsTestUser, :upsert) == true
    assert EtsDataLayer.can?(EtsTestUser, :boolean_filter) == true
    assert EtsDataLayer.can?(EtsTestUser, :transact) == false
    assert EtsDataLayer.can?(EtsTestUser, :delete_with_query) == false
    assert EtsDataLayer.can?(EtsTestUser, :create) == true
    assert EtsDataLayer.can?(EtsTestUser, :read) == true
    assert EtsDataLayer.can?(EtsTestUser, :update) == true
    assert EtsDataLayer.can?(EtsTestUser, :destroy) == true
    assert EtsDataLayer.can?(EtsTestUser, :sort) == true
    assert EtsDataLayer.can?(EtsTestUser, :filter) == true
    assert EtsDataLayer.can?(EtsTestUser, :limit) == true
    assert EtsDataLayer.can?(EtsTestUser, :offset) == true
    assert EtsDataLayer.can?(EtsTestUser, {:filter_predicate, :foo, %In{}}) == true
    assert EtsDataLayer.can?(EtsTestUser, {:filter_predicate, :foo, %Eq{}}) == true
    assert EtsDataLayer.can?(EtsTestUser, {:filter_predicate, :foo, %LessThan{}}) == true
    assert EtsDataLayer.can?(EtsTestUser, {:filter_predicate, :foo, %GreaterThan{}}) == true
    assert EtsDataLayer.can?(EtsTestUser, {:sort, :foo}) == true
    assert EtsDataLayer.can?(EtsTestUser, :foo) == false
  end

  test "resource_to_query" do
    assert %Query{resource: EtsTestUser} = EtsDataLayer.resource_to_query(EtsTestUser)
  end

  test "limit, offset, filter, sortm, aggregate" do
    query = EtsDataLayer.resource_to_query(EtsTestUser)
    assert {:ok, %Query{limit: 3}} = EtsDataLayer.limit(query, 3, :foo)
    assert {:ok, %Query{offset: 10}} = EtsDataLayer.offset(query, 10, :foo)
    assert {:ok, %Query{filter: :all}} = EtsDataLayer.filter(query, :all, :foo)
    assert {:ok, %Query{sort: :asc}} = EtsDataLayer.sort(query, :asc, :foo)
    assert {:ok, %Query{aggregates: [:foo]}} = EtsDataLayer.add_aggregate(query, :foo, :bar)
  end

  test "create" do
    assert %EtsTestUser{id: id, name: "Mike"} = create_user(%{name: "Mike"})

    assert [{%{id: ^id}, %EtsTestUser{name: "Mike", id: ^id}}] = user_table()
  end

  test "update" do
    %EtsTestUser{id: id} = user = create_user(%{name: "Mike"})

    assert [{%{id: ^id}, %EtsTestUser{id: ^id, name: "Mike"}}] = user_table()

    user
    |> Ash.Changeset.new(name: "Joe")
    |> EtsApiTest.update!()

    assert [{%{id: ^id}, %EtsTestUser{name: "Joe", id: ^id}}] = user_table()
  end

  test "upsert" do
    %EtsTestUser{id: id} = create_user(%{name: "Mike"}, upsert?: true)

    assert [{%{id: ^id}, %EtsTestUser{id: ^id, name: "Mike"}}] = user_table()

    create_user(%{name: "Joe", id: id}, upsert?: true)

    assert [{%{id: ^id}, %EtsTestUser{name: "Joe", id: ^id}}] = user_table()
  end

  test "destroy" do
    mike = create_user(%{name: "Mike"})
    %EtsTestUser{id: joes_id} = joe = create_user(%{name: "Joe"})

    assert length(user_table()) == 2

    EtsApiTest.destroy!(mike)

    assert [{%{id: ^joes_id}, ^joe}] = user_table()
  end

  test "get" do
    create_user(%{name: "Mike"})
    create_user(%{name: "Joe"})
    %{id: id} = create_user(%{name: "Matthew"})
    create_user(%{name: "Zachary"})

    assert %EtsTestUser{id: ^id, name: "Matthew"} = EtsApiTest.get!(EtsTestUser, id)
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

    assert [^joe, ^matthew, ^mike, ^zachary] = EtsApiTest.read!(query)
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

    assert [^joe, ^matthew] = EtsApiTest.read!(query)
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

    assert [^matthew, ^mike, ^zachary] = EtsApiTest.read!(query)
  end

  describe "filter" do
    setup do
      mike = create_user(%{name: "Mike", age: 37})
      joe = create_user(%{name: "Joe", age: 11})
      matthew = create_user(%{name: "Matthew", age: 9})
      zachary = create_user(%{name: "Zachary", age: 6})
      %{mike: mike, zachary: zachary, matthew: matthew, joe: joe}
    end

    test "values", %{zachary: zachary, matthew: matthew, joe: joe} do
      assert [^zachary] = filter_users(name: "Zachary")
      assert [^joe] = filter_users(name: "Joe")
      assert [^matthew] = filter_users(age: 9)
    end

    test "or, in, eq", %{mike: mike, zachary: zachary, joe: joe} do
      assert [^joe, ^mike, ^zachary] =
               filter_users(
                 or: [
                   [name: [in: ["Zachary", "Mike"]]],
                   [age: [eq: 11]]
                 ]
               )
    end

    test "and, in, eq", %{mike: mike} do
      assert [^mike] =
               filter_users(
                 and: [
                   [name: [in: ["Zachary", "Mike"]]],
                   [age: [eq: 37]]
                 ]
               )
    end

    test "and, in, not", %{zachary: zachary} do
      assert [^zachary] =
               filter_users(
                 and: [
                   [name: [in: ["Zachary", "Mike"]]],
                   [not: [age: 37]]
                 ]
               )
    end

    test "gt", %{mike: mike, joe: joe} do
      assert [^joe, ^mike] = filter_users(age: [gt: 10])
    end

    test "lt", %{zachary: zachary, matthew: matthew} do
      assert [^matthew, ^zachary] = filter_users(age: [lt: 10])
    end

    test "boolean", %{zachary: zachary, matthew: matthew} do
      assert [^matthew, ^zachary] = filter_users(and: [true, age: [lt: 10]])
    end
  end

  defp filter_users(filter) do
    EtsTestUser
    |> Ash.Query.new()
    |> Ash.Query.sort(:name)
    |> Ash.Query.filter(filter)
    |> EtsApiTest.read!()
  end

  defp create_user(attrs, opts \\ []) do
    %EtsTestUser{}
    |> Ash.Changeset.new(attrs)
    |> EtsApiTest.create!(opts)
  end

  defp user_table do
    EtsTestUser
    |> ETS.Set.wrap_existing!()
    |> ETS.Set.to_list!()
  end
end
