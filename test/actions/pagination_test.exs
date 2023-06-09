defmodule Ash.Actions.PaginationTest do
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      attribute :user_id, :uuid
      attribute :body, :string
    end

    actions do
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        pagination offset?: true, required?: true, default_limit: 25
      end
    end

    relationships do
      belongs_to :user, Ash.Actions.PaginationTest.User, define_attribute?: false
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :offset do
        pagination offset?: true, countable: true, required?: true
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

      defaults [:create, :update]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :subname, :string
    end

    aggregates do
      count :count_of_posts, :posts
    end

    calculations do
      calculate :name_with_arg, :string, expr(name) do
        argument :does_nothing, :boolean
      end
    end

    relationships do
      has_many :posts, Post
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry User
      entry Post
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
        user = Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))

        if i != 0 do
          for x <- 1..i do
            Api.create!(Ash.Changeset.new(Post, %{body: "#{i}-#{x}", user_id: user.id}))
          end
        end
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

    test "can include a full count with an offset" do
      assert Api.read!(User, action: :optional_offset, page: [offset: 5, limit: 1, count: true]).count ==
               10
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

  describe "keyset pagination with nil fields" do
    setup do
      users =
        for i <- 0..9 do
          if rem(i, 2) == 0 do
            Api.create!(Ash.Changeset.new(User, %{name: "#{i}", subname: "#{i}"}))
          else
            Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))
          end
        end

      [users: users]
    end

    test "can be paged through when a non-nil value is the keyset" do
      %{results: first_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Api.read!(action: :keyset, page: [limit: 5])

      assert Enum.map(first_results, & &1.name) == ~w(0 2 4 6 8)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Api.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(1 3 5 7 9)
    end

    test "can be paged through  when a nil value is the keyset" do
      %{results: first_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Api.read!(action: :keyset, page: [limit: 6])

      assert Enum.map(first_results, & &1.name) == ~w(0 2 4 6 8 1)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Api.read!(action: :keyset, page: [limit: 6, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(3 5 7 9)
    end

    test "can be paged through when a non-nil value is the keyset using nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Api.read!(action: :keyset, page: [limit: 5])

      assert Enum.map(first_results, & &1.name) == ~w(1 3 5 7 9)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Api.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(0 2 4 6 8)
    end

    test "can be paged through  when a nil value is the keyset using nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Api.read!(action: :keyset, page: [limit: 6])

      assert Enum.map(first_results, & &1.name) == ~w(1 3 5 7 9 0)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Api.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(2 4 6 8)
    end
  end

  describe "keyset pagination" do
    setup do
      users =
        for i <- 0..9 do
          user = Api.create!(Ash.Changeset.new(User, %{name: "#{i}"}))

          if i != 0 do
            for x <- 1..i do
              Api.create!(Ash.Changeset.new(Post, %{body: "#{i}-#{x}", user_id: user.id}))
            end
          end
        end

      [users: users]
    end

    test "can be limited" do
      assert Enum.count(Api.read!(User, action: :optional_keyset, page: false)) == 10
      assert Enum.count(Api.read!(User, action: :optional_keyset, page: [limit: 5]).results) == 5
    end

    test "can include a full count" do
      assert Api.read!(User, action: :optional_keyset, page: [limit: 1, count: true]).count == 10
    end

    test "can include a full count with a sort and limit" do
      assert 10 =
               User
               |> Ash.Query.sort(:name)
               |> Api.read!(action: :optional_keyset, page: [limit: 1, count: true])
               |> Map.get(:count)
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

    test "can get the full count when asking for records after a specific keyset" do
      %{results: [%{__metadata__: %{keyset: keyset}}], count: 10} =
        Api.read!(User, action: :keyset, page: [count: true, limit: 1])

      assert %{count: 10} =
               Api.read!(User, action: :keyset, page: [count: true, limit: 1, after: keyset])
    end

    test "an invalid keyset returns an appropriate error" do
      assert_raise(Ash.Error.Invalid, ~r/Invalid value provided as a keyset/, fn ->
        Api.read!(User, action: :keyset, page: [limit: 1, after: "~"])
      end)
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

    test "can ask for records before a specific keyset, with the sort order honored" do
      %{results: users} =
        User |> Ash.Query.sort(:name) |> Api.read!(action: :keyset, page: [limit: 100])

      users = Enum.sort_by(users, & &1.name)
      last_user = List.last(users)

      %{results: results} =
        User
        |> Ash.Query.sort(:name)
        |> Api.read!(action: :keyset, page: [limit: 2, before: last_user.__metadata__.keyset])

      assert Enum.map(results, & &1.name) == [
               "7",
               "8"
             ]
    end

    test "can ask for records before a specific keyset, with the full count shown" do
      %{results: users} =
        User |> Ash.Query.sort(:name) |> Api.read!(action: :keyset, page: [limit: 100])

      users = Enum.sort_by(users, & &1.name)
      last_user = List.last(users)
      assert Enum.count(users) == 10

      assert %{count: 10} =
               User
               |> Ash.Query.sort(:name)
               |> Api.read!(
                 action: :keyset,
                 page: [count: true, limit: 2, before: last_user.__metadata__.keyset]
               )
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

    test "pagination works with a :desc sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "4")
        |> Ash.Query.sort(name: :desc)
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

    test "pagination works with a sort applied that uses an aggregate" do
      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Api.read!(page: [after: keyset, limit: 4])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8"]
      assert page.more?
    end

    test "pagination more? is false when there are no more records" do
      page =
        User
        |> Ash.Query.filter(count_of_posts == 5)
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Api.read!(page: [after: keyset, limit: 4])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["6", "7", "8", "9"]
      refute page.more?
    end

    test "pagination works with a sort applied that uses an aggregate using `before`" do
      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Api.read!(page: [before: keyset, limit: 3])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["1", "2", "3"]
      assert page.more?

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Api.read!(page: [after: keyset, limit: 4])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8"]
      assert page.more?
    end

    test "pagination more? is false when there are no more records using `before`" do
      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Api.read!(page: [before: keyset, limit: 4])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["0", "1", "2", "3"]
      refute page.more?
    end

    test "pagination works with a sort applied that uses an aggregate desc" do
      User
      |> Ash.Query.load(:count_of_posts)
      |> Api.read!(page: [limit: 10])
      |> Map.get(:results)
      |> Enum.map(&{&1.name, &1.count_of_posts})

      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(count_of_posts: :desc)
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
    end

    test "pagination works with a sort applied that uses a calculation with arguments" do
      page =
        User
        |> Ash.Query.filter(name_with_arg == "4")
        |> Ash.Query.sort(name_with_arg: %{does_nothing: true})
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name_with_arg: %{does_nothing: true})
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a sort applied that uses a calculation desc" do
      page =
        User
        |> Ash.Query.filter(name_with_arg == "4")
        |> Ash.Query.sort(name_with_arg: {:desc, %{does_nothing: true}})
        |> Api.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name_with_arg: {:desc, %{does_nothing: true}})
        |> Api.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
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

    test "the prev request right after the initial query remains the same as the initial result (like offset pagination)" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Api.read!(action: :keyset, page: [limit: 1])

      assert %{results: [%{name: "4"}]} = page = Api.page!(page, :prev)
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

  describe "loading with pagination" do
    test "it does not paginate loads" do
      user = Api.create!(Ash.Changeset.new(User, %{name: "user"}))
      Api.create!(Ash.Changeset.new(Post, %{user_id: user.id}))

      assert [_ | _] =
               user
               |> Api.load!([posts: :user], tenant: nil, actor: nil)
               |> Map.get(:posts)
    end
  end
end
