defmodule Ash.Actions.PaginationTest do
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :offset do
        pagination offset?: true, countable: true
      end

      read :optional_offset do
        pagination offset?: true, countable: true, required?: false
      end

      read :offset_countable_by_default do
        pagination offset?: true, countable: :by_default, required?: false
      end

      read :required_offset_with_default do
        pagination offset?: true, countable: true, required?: false, default_limit: 25
      end

      read :keyset do
        pagination keyset?: true, countable: true
      end

      read :optional_keyset do
        pagination keyset?: true, countable: true, required?: false
      end

      read :keyset_countable_by_default do
        pagination keyset?: true, countable: :by_default, required?: false
      end

      read :required_keyset_with_default do
        pagination keyset?: true, countable: true, required?: false, default_limit: 25
      end

      read :both_required do
        primary? true
        pagination keyset?: true, offset?: true, countable: true
      end

      read :both_optional do
        pagination keyset?: true, offset?: true, countable: true, default_limit: 25
      end

      create :create
      update :update
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
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
    use Ash.Api

    resources do
      registry Registry
    end
  end

  test "pagination is required by default" do
    assert_raise Ash.Error.Invalid, ~r/Pagination is required/, fn ->
      Api.read!(User, page: false)
    end
  end

  test "a default limit allows not specifying page parameters" do
    assert_raise Ash.Error.Invalid, ~r/Limit is required/, fn ->
      Api.read!(User, page: [offset: 1])
    end

    Api.read!(User, action: :required_offset_with_default)
  end

  describe "offset pagination" do
    setup do
      for i <- 0..9 do
        Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))
      end

      :ok
    end

    test "can be limited" do
      assert Enum.count(Api.read!(User, action: :optional_offset, page: false)) == 10
      assert Enum.count(Api.read!(User, action: :optional_offset, page: [limit: 5]).results) == 5
    end

    test "can be offset" do
      assert Enum.count(Api.read!(User, action: :optional_offset, page: false)) == 10

      assert Enum.count(
               Api.read!(User, action: :optional_offset, page: [offset: 5, limit: 5]).results
             ) == 5
    end

    test "can include a full count" do
      assert Api.read!(User, action: :optional_offset, page: [limit: 1, count: true]).count == 10
    end

    test "can default to including a count" do
      assert Api.read!(User, action: :offset_countable_by_default, page: [limit: 1]).count == 10
    end

    test "count is not included by default otherwise" do
      assert is_nil(Api.read!(User, action: :optional_offset, page: [limit: 1]).count)
    end

    test "`count: false` prevents the count from occurring even if it is on `by_default`" do
      assert is_nil(
               Api.read!(User,
                 action: :offset_countable_by_default,
                 page: [limit: 1, count: false]
               ).count
             )
    end

    test "pagination works with a sort applied" do
      names =
        User
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [offset: 5, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a reversed sort applied" do
      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Api.read!(page: [offset: 5, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["4", "3", "2", "1", "0"]
    end

    test "pagination works with a filter" do
      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
        |> Api.read!(page: [offset: 1, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
    end

    test "the next page can be fetched" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [offset: 1, limit: 1])

      assert %{results: [%{name: "2"}]} = Api.page!(page, :next)
    end

    test "the previous page can be fetched" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [offset: 1, limit: 1])

      assert %{results: [%{name: "4"}]} = Api.page!(page, :prev)
    end

    test "the first page can be fetched" do
      assert %{results: [%{name: "2"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [offset: 2, limit: 1])

      assert %{results: [%{name: "4"}]} = Api.page!(page, :first)
    end

    test "the last page can be fetched if the count was requested" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [offset: 1, limit: 1, count: true])

      assert %{results: [%{name: "0"}]} = Api.page!(page, :last)
    end
  end

  describe "keyset pagination" do
    setup do
      for i <- 0..9 do
        Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))
      end

      :ok
    end

    test "can be limited" do
      assert Enum.count(Api.read!(User, action: :optional_keyset, page: false)) == 10
      assert Enum.count(Api.read!(User, action: :optional_keyset, page: [limit: 5]).results) == 5
    end

    test "can include a full count" do
      assert Api.read!(User, action: :optional_keyset, page: [limit: 1, count: true]).count == 10
    end

    test "can default to including a count" do
      assert Api.read!(User, action: :keyset_countable_by_default, page: [limit: 1]).count == 10
    end

    test "count is not included by default otherwise" do
      assert is_nil(Api.read!(User, action: :optional_keyset, page: [limit: 1]).count)
    end

    test "`count: false` prevents the count from occurring even if it is on `by_default`" do
      assert is_nil(
               Api.read!(User,
                 action: :keyset_countable_by_default,
                 page: [limit: 1, count: false]
               ).count
             )
    end

    test "can ask for records after a specific keyset" do
      %{results: [%{id: id, __metadata__: %{keyset: keyset}}]} =
        Api.read!(User, action: :keyset, page: [limit: 1])

      %{results: [%{id: next_id}]} =
        Api.read!(User, action: :keyset, page: [limit: 1, after: keyset])

      refute id == next_id
    end

    test "can ask for records before a specific keyset" do
      %{results: [%{id: id, __metadata__: %{keyset: keyset}}]} =
        Api.read!(User, action: :keyset, page: [limit: 1])

      %{results: [%{id: next_id, __metadata__: %{keyset: keyset2}}]} =
        Api.read!(User, action: :keyset, page: [limit: 1, after: keyset])

      refute id == next_id

      %{results: [%{id: before_id}]} =
        Api.read!(User, action: :keyset, page: [limit: 1, before: keyset2])

      assert id == before_id
    end

    test "pagination works with a sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "4")
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a reversed sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "5")
        |> Ash.Query.sort(name: :desc)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["4", "3", "2", "1", "0"]
    end

    test "pagination works with a filter" do
      page =
        User
        |> Ash.Query.filter(name == "5")
        |> Ash.Query.sort(name: :desc)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.Query.filter(name != "4")
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
    end

    test "the next page can be fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [limit: 1])

      assert %{results: [%{name: "3"}]} = Api.page!(page, :next)
    end

    test "the previous page can be fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [limit: 1], action: :optional_keyset)

      assert %{results: [%{name: "3"}]} = page = Api.page!(page, :next)
      assert %{results: [%{name: "4"}]} = Api.page!(page, :prev)
    end

    test "the first page can be fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(page: [limit: 1])

      assert %{results: [%{name: "3"}]} = page = Api.page!(page, :next)
      assert %{results: [%{name: "4"}]} = Api.page!(page, :first)
    end
  end

  describe "when both are supported" do
    setup do
      for i <- 0..9 do
        Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))
      end

      :ok
    end

    test "it defaults to offset pagination" do
      assert %Ash.Page.Offset{} = Api.read!(User, action: :both_optional, page: [limit: 10])
    end

    test "it adds a keyset to the records, even though it returns an offset page" do
      for result <- Api.read!(User, action: :both_optional, page: [limit: 10]).results do
        refute is_nil(result.__metadata__.keyset)
      end
    end
  end
end
