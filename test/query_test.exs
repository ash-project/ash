defmodule Ash.Test.QueryTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

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
end
