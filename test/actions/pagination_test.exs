defmodule Ash.Actions.PaginationTest do
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id

      attribute :user_id, :uuid do
        public?(true)
      end

      attribute :body, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:create, :update, :destroy]

      read :read do
        primary? true
        pagination offset?: true, required?: true, default_limit: 25
      end
    end

    relationships do
      belongs_to :user, Ash.Actions.PaginationTest.User, define_attribute?: false, public?: true
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*

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

      read :keyset_before_action do
        prepare(before_action(&Ash.Query.filter(&1, name in ["0", "1", "2"])))
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

      attribute :name, :string do
        public?(true)
      end

      attribute :subname, :string do
        public?(true)
      end
    end

    aggregates do
      count :count_of_posts, :posts do
        public? true
      end
    end

    calculations do
      calculate :name_with_arg, :string, expr(name) do
        public?(true)

        argument :does_nothing, :boolean do
          allow_nil? false
        end
      end
    end

    relationships do
      has_many :posts, Post do
        public?(true)
      end
    end
  end

  test "pagination is required by default" do
    assert_raise Ash.Error.Invalid, ~r/Pagination is required/, fn ->
      Ash.read!(User, page: false)
    end
  end

  test "a default limit allows not specifying page parameters" do
    assert_raise Ash.Error.Invalid, ~r/Limit is required/, fn ->
      Ash.read!(User, page: [offset: 1])
    end

    Ash.read!(User, action: :required_offset_with_default)
  end

  describe "offset pagination" do
    setup do
      for i <- 0..9 do
        user = Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "#{i}"}))

        if i != 0 do
          for x <- 1..i do
            Ash.create!(
              Ash.Changeset.for_create(Post, :create, %{body: "#{i}-#{x}", user_id: user.id})
            )
          end
        end
      end

      :ok
    end

    test "can be limited" do
      assert Enum.count(Ash.read!(User, action: :optional_offset, page: false)) == 10

      assert Enum.count(Ash.read!(User, action: :optional_offset, page: [limit: 5]).results) ==
               5
    end

    test "can be offset" do
      assert Enum.count(Ash.read!(User, action: :optional_offset, page: false)) == 10

      assert Enum.count(
               Ash.read!(User, action: :optional_offset, page: [offset: 5, limit: 5]).results
             ) == 5
    end

    test "can include a full count" do
      assert Ash.read!(User, action: :optional_offset, page: [limit: 1, count: true]).count ==
               10
    end

    test "can include a full count with an offset" do
      assert Ash.read!(User,
               action: :optional_offset,
               page: [offset: 5, limit: 1, count: true]
             ).count ==
               10
    end

    test "can default to including a count" do
      assert Ash.read!(User, action: :offset_countable_by_default, page: [limit: 1]).count ==
               10
    end

    test "count is not included by default otherwise" do
      assert is_nil(Ash.read!(User, action: :optional_offset, page: [limit: 1]).count)
    end

    test "`count: false` prevents the count from occurring even if it is on `by_default`" do
      assert is_nil(
               Ash.read!(User,
                 action: :offset_countable_by_default,
                 page: [limit: 1, count: false]
               ).count
             )
    end

    test "pagination works with a sort applied" do
      names =
        User
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [offset: 5, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a reversed sort applied" do
      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.read!(page: [offset: 5, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["4", "3", "2", "1", "0"]
    end

    test "pagination works with a filter" do
      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
        |> Ash.read!(page: [offset: 1, limit: 5])
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
               |> Ash.read!(page: [offset: 1, limit: 1])

      assert %{results: [%{name: "2"}]} = Ash.page!(page, :next)
    end

    test "the previous page can be fetched" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [offset: 1, limit: 1])

      assert %{results: [%{name: "4"}]} = Ash.page!(page, :prev)
    end

    test "the first page can be fetched" do
      assert %{results: [%{name: "2"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [offset: 2, limit: 1])

      assert %{results: [%{name: "4"}]} = Ash.page!(page, :first)
    end

    test "the last page can be fetched if the count was requested" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [offset: 1, limit: 1, count: true])

      assert %{results: [%{name: "0"}]} = Ash.page!(page, :last)
    end

    test "the same page can be re-fetched" do
      assert %{results: [%{name: "3"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [offset: 1, limit: 1, count: true])

      assert %{results: [%{name: "3"}]} = Ash.page!(page, :self)
    end
  end

  describe "keyset pagination with nil fields" do
    setup do
      users =
        for i <- 0..9 do
          if rem(i, 2) == 0 do
            Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "#{i}", subname: "#{i}"}))
          else
            Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "#{i}"}))
          end
        end

      [users: users]
    end

    test "can be paged through when a non-nil value is the keyset" do
      %{results: first_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Ash.Query.select([:name])
        |> Ash.read!(action: :keyset, page: [limit: 5])

      assert Enum.map(first_results, & &1.name) == ~w(0 2 4 6 8)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Ash.Query.select([:name])
        |> Ash.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(1 3 5 7 9)
    end

    test "can be paged through  when a nil value is the keyset" do
      %{results: first_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Ash.read!(action: :keyset, page: [limit: 6])

      assert Enum.map(first_results, & &1.name) == ~w(0 2 4 6 8 1)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort([:subname, :name])
        |> Ash.read!(action: :keyset, page: [limit: 6, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(3 5 7 9)
    end

    test "can be paged through when a non-nil value is the keyset using asc_nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5])

      assert Enum.map(first_results, & &1.name) == ~w(1 3 5 7 9)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(0 2 4 6 8)
    end

    test "can be paged through when a non-nil value is the keyset using desc_nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :desc_nils_first, name: :desc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5])

      assert Enum.map(first_results, & &1.name) == ~w(9 7 5 3 1)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :desc_nils_first, name: :desc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(8 6 4 2 0)
    end

    test "can be paged through  when a nil value is the keyset using asc_nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 6])

      assert Enum.map(first_results, & &1.name) == ~w(1 3 5 7 9 0)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :asc_nils_first, name: :asc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(2 4 6 8)
    end

    test "can be paged through  when a nil value is the keyset using desc_nils_first" do
      %{results: first_results} =
        User
        |> Ash.Query.sort(subname: :desc_nils_first, name: :desc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 6])

      assert Enum.map(first_results, & &1.name) == ~w(9 7 5 3 1 8)

      keyset =
        first_results |> List.last(first_results) |> Map.get(:__metadata__) |> Map.get(:keyset)

      %{results: second_results} =
        User
        |> Ash.Query.sort(subname: :desc_nils_first, name: :desc_nils_first)
        |> Ash.read!(action: :keyset, page: [limit: 5, after: keyset])

      assert Enum.map(second_results, & &1.name) == ~w(6 4 2 0)
    end
  end

  describe "keyset pagination" do
    setup do
      users =
        for i <- 0..9 do
          user = Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "#{i}"}))

          if i != 0 do
            for x <- 1..i do
              Ash.create!(
                Ash.Changeset.for_create(Post, :create, %{body: "#{i}-#{x}", user_id: user.id})
              )
            end
          end
        end

      [users: users]
    end

    test "can be limited" do
      assert Enum.count(Ash.read!(User, action: :optional_keyset, page: false)) == 10

      assert Enum.count(Ash.read!(User, action: :optional_keyset, page: [limit: 5]).results) ==
               5
    end

    test "can include a full count" do
      assert Ash.read!(User, action: :optional_keyset, page: [limit: 1, count: true]).count ==
               10
    end

    test "can include a full count with a sort and limit" do
      assert 10 =
               User
               |> Ash.Query.sort(:name)
               |> Ash.read!(action: :optional_keyset, page: [limit: 1, count: true])
               |> Map.get(:count)
    end

    test "can default to including a count" do
      assert Ash.read!(User, action: :keyset_countable_by_default, page: [limit: 1]).count ==
               10
    end

    test "count is not included by default otherwise" do
      assert is_nil(Ash.read!(User, action: :optional_keyset, page: [limit: 1]).count)
    end

    test "`count: false` prevents the count from occurring even if it is on `by_default`" do
      assert is_nil(
               Ash.read!(User,
                 action: :keyset_countable_by_default,
                 page: [limit: 1, count: false]
               ).count
             )
    end

    test "can ask for records after a specific keyset" do
      %{results: [%{id: id, __metadata__: %{keyset: keyset}}]} =
        Ash.read!(User, action: :keyset, page: [limit: 1])

      %{results: [%{id: next_id}]} =
        Ash.read!(User, action: :keyset, page: [limit: 1, after: keyset])

      refute id == next_id
    end

    test "can get the full count when asking for records after a specific keyset" do
      %{results: [%{__metadata__: %{keyset: keyset}}], count: 10} =
        Ash.read!(User, action: :keyset, page: [count: true, limit: 1])

      assert %{count: 10} =
               Ash.read!(User, action: :keyset, page: [count: true, limit: 1, after: keyset])
    end

    test "can get the full count when asking for records after a specific keyset use the query after applying `before_action` hooks" do
      %{results: [%{__metadata__: %{keyset: keyset}}], count: 3} =
        Ash.read!(User, action: :keyset_before_action, page: [count: true, limit: 1])

      assert %{count: 3} =
               Ash.read!(User,
                 action: :keyset_before_action,
                 page: [count: true, limit: 1, after: keyset]
               )
    end

    test "an invalid keyset returns an appropriate error" do
      assert_raise(Ash.Error.Invalid, ~r/Invalid value provided as a keyset/, fn ->
        Ash.read!(User, action: :keyset, page: [limit: 1, after: "~"])
      end)
    end

    test "can ask for records before a specific keyset" do
      %{results: [%{id: id, __metadata__: %{keyset: keyset}}]} =
        Ash.read!(User, action: :keyset, page: [limit: 1])

      %{results: [%{id: next_id, __metadata__: %{keyset: keyset2}}]} =
        Ash.read!(User, action: :keyset, page: [limit: 1, after: keyset])

      refute id == next_id

      %{results: [%{id: before_id}]} =
        Ash.read!(User, action: :keyset, page: [limit: 1, before: keyset2])

      assert id == before_id
    end

    test "can ask for records before a specific keyset, with the sort order honored" do
      %{results: users} =
        User |> Ash.Query.sort(:name) |> Ash.read!(action: :keyset, page: [limit: 100])

      users = Enum.sort_by(users, & &1.name)
      last_user = List.last(users)

      %{results: results} =
        User
        |> Ash.Query.sort(:name)
        |> Ash.read!(action: :keyset, page: [limit: 2, before: last_user.__metadata__.keyset])

      assert Enum.map(results, & &1.name) == [
               "7",
               "8"
             ]
    end

    test "can ask for records before a specific keyset, with the full count shown" do
      %{results: users} =
        User |> Ash.Query.sort(:name) |> Ash.read!(action: :keyset, page: [limit: 100])

      users = Enum.sort_by(users, & &1.name)
      last_user = List.last(users)
      assert Enum.count(users) == 10

      assert %{count: 10} =
               User
               |> Ash.Query.sort(:name)
               |> Ash.read!(
                 action: :keyset,
                 page: [count: true, limit: 2, before: last_user.__metadata__.keyset]
               )
    end

    test "pagination works with a sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "4")
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a :desc sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "4")
        |> Ash.Query.sort(name: :desc)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination works with a sort applied that uses an aggregate" do
      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Ash.read!(page: [after: keyset, limit: 4])

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
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Ash.read!(page: [after: keyset, limit: 4])

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
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Ash.read!(page: [before: keyset, limit: 3])

      names =
        page
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["1", "2", "3"]
      assert page.more?

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Ash.read!(page: [after: keyset, limit: 4])

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
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      page =
        User
        |> Ash.Query.sort(:count_of_posts)
        |> Ash.read!(page: [before: keyset, limit: 4])

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
      |> Ash.read!(page: [limit: 10])
      |> Map.get(:results)
      |> Enum.map(&{&1.name, &1.count_of_posts})

      page =
        User
        |> Ash.Query.filter(count_of_posts == 4)
        |> Ash.Query.sort(:name)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(count_of_posts: :desc)
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
    end

    test "pagination works with a sort applied that uses a calculation with arguments" do
      page =
        User
        |> Ash.Query.filter(name_with_arg(does_nothing: true) == "4")
        |> Ash.Query.sort(name_with_arg: %{does_nothing: true})
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name_with_arg: %{does_nothing: true})
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["5", "6", "7", "8", "9"]
    end

    test "pagination utilities work with a sort applied that uses a calculation with arguments" do
      assert %{results: [%{name_with_arg: "5"}]} =
               User
               |> Ash.Query.sort(name_with_arg: %{does_nothing: true})
               |> Ash.Query.load(name_with_arg: %{does_nothing: true})
               |> Ash.read!(page: [limit: 1, offset: 4])
               |> Ash.page!(:next)
    end

    test "pagination works with a sort applied that uses a calculation desc" do
      page =
        User
        |> Ash.Query.filter(name_with_arg(does_nothing: true) == "4")
        |> Ash.Query.sort(name_with_arg: {:desc, %{does_nothing: true}})
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name_with_arg: {:desc, %{does_nothing: true}})
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["3", "2", "1", "0"]
    end

    test "pagination works with a reversed sort applied" do
      page =
        User
        |> Ash.Query.filter(name == "5")
        |> Ash.Query.sort(name: :desc)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.read!(page: [after: keyset, limit: 5])
        |> Map.get(:results)
        |> Enum.map(& &1.name)

      assert names == ["4", "3", "2", "1", "0"]
    end

    test "pagination works with a filter" do
      page =
        User
        |> Ash.Query.filter(name == "5")
        |> Ash.Query.sort(name: :desc)
        |> Ash.read!(page: [limit: 1])

      keyset = Enum.at(page.results, 0).__metadata__.keyset

      names =
        User
        |> Ash.Query.sort(name: :desc)
        |> Ash.Query.filter(name != "4")
        |> Ash.read!(page: [after: keyset, limit: 5])
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
               |> Ash.read!(page: [limit: 1])

      assert %{results: [%{name: "3"}]} = Ash.page!(page, :next)
    end

    test "the previous page can be fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [limit: 1], action: :optional_keyset)

      assert %{results: [%{name: "3"}]} = page = Ash.page!(page, :next)
      assert %{results: [%{name: "4"}]} = Ash.page!(page, :prev)
    end

    test "the first page can be fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [limit: 1])

      assert %{results: [%{name: "3"}]} = page = Ash.page!(page, :next)
      assert %{results: [%{name: "4"}]} = Ash.page!(page, :first)
    end

    test "the same page can be re-fetched" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(page: [limit: 1])

      assert %{results: [%{name: "3"}]} = page = Ash.page!(page, :next)
      assert %{results: [%{name: "3"}]} = Ash.page!(page, :self)
    end

    test "the prev request right after the initial query remains the same as the initial result (like offset pagination)" do
      assert %{results: [%{name: "4"}]} =
               page =
               User
               |> Ash.Query.sort(name: :desc)
               |> Ash.Query.filter(name in ["4", "3", "2", "1", "0"])
               |> Ash.read!(action: :keyset, page: [limit: 1])

      assert %{results: [%{name: "4"}]} = Ash.page!(page, :prev)
    end
  end

  describe "when both are supported" do
    setup do
      for i <- 0..9 do
        Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "#{i}"}))
      end

      :ok
    end

    test "it defaults to offset pagination" do
      assert %Ash.Page.Offset{} = Ash.read!(User, action: :both_optional, page: [limit: 10])
    end

    test "it adds a keyset to the records, even though it returns an offset page" do
      for result <- Ash.read!(User, action: :both_optional, page: [limit: 10]).results do
        refute is_nil(result.__metadata__.keyset)
      end
    end
  end

  describe "loading with pagination" do
    test "it does not paginate loads" do
      user = Ash.create!(Ash.Changeset.for_create(User, :create, %{name: "user"}))
      Ash.create!(Ash.Changeset.for_create(Post, :create, %{user_id: user.id}))

      assert [_ | _] =
               user
               |> Ash.load!([posts: :user], tenant: nil, actor: nil)
               |> Map.get(:posts)
    end
  end
end
