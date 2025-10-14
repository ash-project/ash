# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.QueryTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query
  import Ash.Expr

  alias Ash.Test.Domain, as: Domain

  defmodule User do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults create: :*, update: :*

      read :read do
        primary? true
      end

      read :by_id do
        argument :id, :uuid, allow_nil?: false

        filter expr(id == ^arg(:id))
      end

      read :list_by_name_substring do
        argument :name_substring, :string do
          constraints min_length: 4
        end

        filter expr(contains(name, ^arg(:name_substring)))
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :list, {:array, :string}, public?: true

      attribute :name, :string do
        public?(true)
      end

      attribute :email, :string, sensitive?: true, public?: true
    end

    relationships do
      belongs_to :best_friend, __MODULE__ do
        public?(true)
      end
    end
  end

  describe "inspect" do
    test "queries can be inspected with or without an action" do
      assert inspect(Ash.Query.new(User)) == "#Ash.Query<resource: Ash.Test.QueryTest.User>"
      assert inspect(Ash.Query.for_read(User, :by_id, %{id: "foobar"})) =~ "action: :by_id"
    end
  end

  describe "read argument validation" do
    test "it returns an appropriate error when an argument is invalid" do
      query = Ash.Query.for_read(User, :by_id, %{id: "foobar"})
      assert [%Ash.Error.Query.InvalidArgument{field: :id}] = query.errors
    end

    test "it returns an appropriate error when an argument constraint is violated" do
      query = Ash.Query.for_read(User, :list_by_name_substring, %{name_substring: "foo"})

      assert [%Ash.Error.Query.InvalidArgument{field: :name_substring, vars: [min: 4]}] =
               query.errors
    end

    test "it ignores unknown inputs with skip_unknown_inputs" do
      query =
        Ash.Query.for_read(User, :by_id, %{id: Ash.UUID.generate(), unknown_input: "ignored"},
          skip_unknown_inputs: :*
        )

      assert query.errors == []
    end
  end

  describe "page validation" do
    test "it returns an appropriate error when a page is invalid" do
      query = Ash.Query.page(User, limit: :foo)
      assert [%Ash.Error.Query.InvalidPage{page: [limit: :foo]}] = query.errors
    end
  end

  describe "sensitive attributes" do
    test "it hides the value in filters" do
      equals = Ash.Query.filter(User, email == "fred") |> inspect()
      refute equals =~ "fred"
      assert equals =~ "**redacted**"

      contains = Ash.Query.filter(User, contains(email, "fred")) |> inspect()
      refute contains =~ "fred"
      assert contains =~ "**redacted**"

      concat = Ash.Query.filter(User, email <> "-fred" == "a@b.com-fred") |> inspect()
      refute concat =~ "fred"
      assert concat =~ "**redacted**"
    end
  end

  describe "loading?" do
    test "it detects a simple load" do
      assert User
             |> Ash.Query.load(:best_friend)
             |> Ash.Query.loading?(:best_friend)
    end
  end

  describe "combinations" do
    test "it combines multiple queries into one result set" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "fred", email: "b@bar.com"})
      Ash.create!(User, %{name: "fred", email: "c@bar.com"})
      Ash.create!(User, %{name: "fred", email: "d@baz.com"})
      Ash.create!(User, %{name: "george"})

      assert [%User{email: "c@bar.com"}, %User{email: "a@bar.com"}] =
               User
               |> Ash.Query.filter(name == "fred")
               |> Ash.Query.combination_of([
                 Ash.Query.Combination.base(
                   filter: expr(contains(email, "bar.com")),
                   limit: 1,
                   sort: [email: :desc]
                 ),
                 Ash.Query.Combination.union_all(
                   filter: expr(contains(email, "bar.com")),
                   limit: 1,
                   sort: [email: :asc]
                 )
               ])
               |> Ash.read!()
    end

    test "you can define computed properties" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "fred", email: "b@bar.com"})
      Ash.create!(User, %{name: "fred", email: "c@bar.com"})
      Ash.create!(User, %{name: "fred", email: "d@baz.com"})
      Ash.create!(User, %{name: "george"})

      assert [%User{email: "c@bar.com", calculations: %{match_group: 1}}] =
               User
               |> Ash.Query.filter(name == "fred")
               |> Ash.Query.combination_of([
                 Ash.Query.Combination.base(
                   filter: expr(contains(email, "bar.com")),
                   limit: 1,
                   calculations: %{
                     match_group: calc(1, type: :integer),
                     same_thing: calc(1, type: :integer)
                   },
                   sort: [email: :desc]
                 ),
                 Ash.Query.Combination.union_all(
                   filter: expr(contains(email, "bar.com")),
                   calculations: %{
                     match_group: calc(2, type: :integer),
                     same_thing: calc(1, type: :integer)
                   },
                   limit: 1,
                   sort: [email: :asc]
                 )
               ])
               |> Ash.Query.distinct_sort([{calc(^combinations(:same_thing)), :asc}])
               |> Ash.Query.sort([{calc(^combinations(:match_group)), :desc}])
               |> Ash.Query.distinct([{calc(^combinations(:same_thing)), :asc}])
               |> Ash.Query.calculate(:match_group, :integer, expr(^combinations(:match_group)))
               |> Ash.read!()
    end

    test "it handles combinations with intersect" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "john", email: "j@bar.com"})
      Ash.create!(User, %{name: "fred", email: "f@baz.com"})
      Ash.create!(User, %{name: "alice", email: "a@bar.com"})

      assert [%User{name: "fred", email: "a@bar.com"}] =
               User
               |> Ash.Query.combination_of([
                 Ash.Query.Combination.base(filter: expr(name == "fred")),
                 Ash.Query.Combination.intersect(filter: expr(contains(email, "bar.com")))
               ])
               |> Ash.read!()
    end

    test "it handles combinations with except" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "fred", email: "b@bar.com"})
      Ash.create!(User, %{name: "john", email: "j@baz.com"})

      result =
        User
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(filter: expr(name == "fred")),
          Ash.Query.Combination.except(filter: expr(contains(email, "b@")))
        ])
        |> Ash.read!()

      assert length(result) == 1
      assert hd(result).email == "a@bar.com"
    end

    test "combinations with multiple union_all" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "alice", email: "a@baz.com"})
      Ash.create!(User, %{name: "john", email: "j@qux.com"})

      result =
        User
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(filter: expr(name == "fred")),
          Ash.Query.Combination.union_all(filter: expr(name == "alice")),
          Ash.Query.Combination.union_all(filter: expr(name == "john"))
        ])
        |> Ash.read!()

      assert length(result) == 3
      assert Enum.any?(result, &(&1.name == "fred"))
      assert Enum.any?(result, &(&1.name == "alice"))
      assert Enum.any?(result, &(&1.name == "john"))
    end

    test "combination with offset" do
      # Create users with ascending email for predictable sort order
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "fred", email: "b@bar.com"})
      Ash.create!(User, %{name: "fred", email: "c@bar.com"})

      result =
        User
        |> Ash.Query.filter(name == "fred")
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(
            filter: expr(contains(email, "bar.com")),
            offset: 1,
            limit: 2,
            sort: [email: :asc]
          )
        ])
        |> Ash.read!()

      assert length(result) == 2
      assert hd(result).email == "b@bar.com"
      assert List.last(result).email == "c@bar.com"
    end

    test "combinations with complex calculations" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "john", email: "j@baz.com"})

      result =
        User
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(
            filter: expr(name == "fred"),
            calculations: %{
              domain: calc("bar", type: :string),
              full_name: calc(name <> "@bar", type: :string)
            }
          ),
          Ash.Query.Combination.union_all(
            filter: expr(name == "john"),
            calculations: %{
              domain: calc("baz", type: :string),
              full_name: calc(name <> "@baz", type: :string)
            }
          )
        ])
        |> Ash.Query.calculate(:email_domain, :string, expr(^combinations(:domain)))
        |> Ash.Query.calculate(:display_name, :string, expr(^combinations(:full_name)))
        |> Ash.read!()

      fred = Enum.find(result, &(&1.name == "fred"))
      john = Enum.find(result, &(&1.name == "john"))

      assert fred.calculations.email_domain == "bar"
      assert fred.calculations.display_name == "fred@bar"
      assert john.calculations.email_domain == "baz"
      assert john.calculations.display_name == "john@baz"
    end

    test "combinations with sorting by calculation" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      Ash.create!(User, %{name: "alice", email: "a@baz.com"})
      Ash.create!(User, %{name: "john", email: "j@qux.com"})

      result =
        User
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(calculations: %{sort_order: calc(3, type: :integer)}),
          Ash.Query.Combination.union_all(
            filter: expr(name == "alice"),
            calculations: %{sort_order: calc(1, type: :integer)}
          ),
          Ash.Query.Combination.union_all(
            filter: expr(name == "john"),
            calculations: %{sort_order: calc(2, type: :integer)}
          )
        ])
        |> Ash.Query.sort([{calc(^combinations(:sort_order)), :asc}, {:name, :asc}])
        |> Ash.Query.distinct(:name)
        |> Ash.read!()

      assert [first, second, third | _] = result
      assert first.name == "alice"
      assert second.name == "john"
      assert third.name == "fred"
    end

    test "combination with distinct" do
      Ash.create!(User, %{name: "fred", email: "a@bar.com"})
      # Same email domain
      Ash.create!(User, %{name: "alice", email: "a@bar.com"})
      Ash.create!(User, %{name: "john", email: "j@baz.com"})

      result =
        User
        |> Ash.Query.combination_of([
          Ash.Query.Combination.base(
            filter: expr(contains(email, "bar.com")),
            select: [:id],
            calculations: %{email_domain: calc("bar.com", type: :string)}
          ),
          Ash.Query.Combination.union_all(
            filter: expr(contains(email, "baz.com")),
            select: [:id],
            calculations: %{email_domain: calc("baz.com", type: :string)}
          )
        ])
        |> Ash.Query.distinct([{calc(^combinations(:email_domain)), :asc}])
        |> Ash.read!()

      # Should only have 2 results since we're distinct on email domain
      assert length(result) == 2

      domains =
        Enum.map(result, fn r ->
          Map.get(r.calculations || %{}, :email_domain)
        end)
        |> Enum.reject(&is_nil/1)

      assert "bar.com" in domains
      assert "baz.com" in domains
    end
  end

  describe "filter" do
    test "can filter by list" do
      list = ["a", "b", "c"]

      Ash.create!(User, %{list: list})

      assert User
             |> Ash.Query.filter(list: list)
             |> Ash.read_one!()
    end
  end

  describe "action validation" do
    test "it fails when the action requested doesn't exist on the resource" do
      assert_raise(ArgumentError, ~r/no such read action/i, fn ->
        Ash.Query.for_read(User, :bananas)
      end)
    end
  end

  describe "adding errors" do
    test "it generates an appropriate exception when adding an error" do
      query = Ash.Query.add_error(User, "", fields: [])
      assert [%Ash.Error.Query.InvalidQuery{}] = query.errors

      query = Ash.Query.add_error(User, "", field: [])
      assert [%Ash.Error.Query.InvalidArgument{}] = query.errors
    end
  end

  describe "aggregate" do
    test "using field option aggregates best friend list" do
      user1 = Ash.create!(User, %{list: nil})
      user2 = Ash.create!(User, %{list: ["a", "b", "c"]})

      Ash.create!(User, %{best_friend_id: user1.id})
      Ash.create!(User, %{best_friend_id: user2.id})

      users =
        User
        |> Ash.Query.aggregate(
          :best_friends_list,
          :first,
          :best_friend,
          field: :list
        )
        |> Ash.read!()

      assert Enum.all?(users, fn user ->
               is_nil(user.aggregates.best_friends_list) or
                 (user.best_friend_id == user2.id and
                    user.aggregates.best_friends_list == ["a", "b", "c"])
             end)
    end
  end
end
