# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Filter.InputPredicateArgumentTypesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Slug do
    @moduledoc false
    use Ash.Type.NewType, subtype_of: :string
  end

  defmodule Status do
    @moduledoc false
    use Ash.Type.Enum, values: [:draft, :published]
  end

  defmodule Bio do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      attribute(:title, :string, public?: true)
    end
  end

  defmodule Engagement do
    @moduledoc false
    use Ash.Type

    @impl true
    def storage_type(_), do: :map

    @impl true
    def composite?(_), do: true

    @impl true
    def composite_types(_), do: [{:likes, :integer, []}, {:shares, :integer, []}]

    @impl true
    def cast_input(nil, _), do: {:ok, nil}

    def cast_input(%{likes: likes, shares: shares}, _)
        when is_integer(likes) and is_integer(shares),
        do: {:ok, %{likes: likes, shares: shares}}

    def cast_input(_, _), do: :error

    @impl true
    def cast_stored(value, constraints), do: cast_input(value, constraints)

    @impl true
    def dump_to_native(value, _), do: {:ok, value}
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key(:id)
      attribute(:title, :string, public?: true)
      attribute(:category, :ci_string, public?: true)
      attribute(:slug, Slug, public?: true)
      attribute(:kind, :atom, public?: true)
      attribute(:status, Status, public?: true)
      attribute(:bio, Bio, public?: true)
      attribute(:engagement, Engagement, public?: true)
      attribute(:score, :integer, public?: true)
      attribute(:tags, {:array, :string}, public?: true)

      create_timestamp(:inserted_at)

      attribute(:valid_at, Ash.Type.Range,
        public?: true,
        constraints: [inner_type: :integer]
      )
    end

    calculations do
      calculate(:likes, :integer, expr(type(get_path(engagement, [:likes]), :integer)))
    end

    actions do
      default_accept(:*)
      defaults([:read, :create])
    end
  end

  setup do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "hello",
        category: "Hello",
        slug: "hello-world",
        kind: :greeting,
        status: :published,
        bio: %{title: "Dr."},
        engagement: %{likes: 1, shares: 2},
        score: 3,
        tags: ["a", "b"],
        valid_at: %Ash.Range{lower: 1, upper: 10}
      })
      |> Ash.create!()

    %{post: post}
  end

  describe "functions whose declared argument type does not match the attribute" do
    test "contains on an integer attribute is an invalid filter error" do
      query = Ash.Query.filter_input(Post, %{"score" => %{"contains" => "x"}})

      assert [%Ash.Error.Query.InvalidFilterValue{}] = query.errors
      assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
    end

    test "string_starts_with on an array attribute is an invalid filter error" do
      query = Ash.Query.filter_input(Post, %{"tags" => %{"string_starts_with" => "a"}})

      assert [%Ash.Error.Query.InvalidFilterValue{}] = query.errors
      assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
    end

    for predicate <- ["range_overlaps", "range_adjacent", "range_contains"] do
      test "#{predicate} on a string attribute is an invalid filter error" do
        query = Ash.Query.filter_input(Post, %{"title" => %{unquote(predicate) => "x"}})

        assert [%Ash.Error.Query.InvalidFilterValue{message: message}] = query.errors
        assert message =~ unquote(predicate)
        assert message =~ "title"
        assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
      end
    end

    test "range_contains holds a point of the inner type" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"valid_at" => %{"range_contains" => 5}})
               |> Ash.read!()

      assert [] =
               Post
               |> Ash.Query.filter_input(%{"valid_at" => %{"range_contains" => 50}})
               |> Ash.read!()
    end

    test "range_contains holds a range of the inner type" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{
                 "valid_at" => %{"range_contains" => %Ash.Range{lower: 2, upper: 3}}
               })
               |> Ash.read!()
    end

    test "the error names the predicate and the attribute" do
      query = Ash.Query.filter_input(Post, %{"score" => %{"contains" => "x"}})

      assert [%Ash.Error.Query.InvalidFilterValue{message: message}] = query.errors
      assert message =~ "contains"
      assert message =~ "score"
      assert message =~ "integer"
    end
  end

  describe "functions whose declared argument type matches or is acted as by the attribute" do
    test "contains on a string attribute" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"title" => %{"contains" => "ell"}})
               |> Ash.read!()
    end

    test "contains on a ci_string attribute" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"category" => %{"contains" => "ELL"}})
               |> Ash.read!()
    end

    test "contains on a NewType of string" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"slug" => %{"contains" => "-wor"}})
               |> Ash.read!()
    end

    test "has on an array attribute" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"tags" => %{"has" => "a"}})
               |> Ash.read!()
    end

    test "contains on an atom attribute, which acts as a string" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"kind" => %{"contains" => "greet"}})
               |> Ash.read!()
    end

    test "contains on an enum attribute, which acts as an atom" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"status" => %{"contains" => "publish"}})
               |> Ash.read!()
    end

    test "at_path on an embedded attribute, which acts as a map" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"bio" => %{"at_path" => ["title"], "eq" => "Dr."}})
               |> Ash.read!()
    end

    test "at_path on a composite attribute, which acts as a map" do
      assert [%Post{}] =
               Post
               |> Ash.Query.filter_input(%{"engagement" => %{"at_path" => ["shares"], "eq" => 2}})
               |> Ash.read!()
    end

    test "get_path on a composite attribute in a calculation, which acts as a map" do
      require Ash.Query

      assert [%Post{likes: 1}] = Post |> Ash.Query.load(:likes) |> Ash.read!()
      assert [%Post{}] = Post |> Ash.Query.filter(likes == 1) |> Ash.read!()
      assert [] = Post |> Ash.Query.filter(likes == 2) |> Ash.read!()
    end
  end

  describe "types that act as another for functions declared narrowly" do
    defmodule Shout do
      @moduledoc false
      # Stands in for a data layer function that only ever declared `:string`.
      use Ash.Query.Function, name: :shout, predicate?: true

      def args, do: [[:string, :string]]
      def returns, do: [:boolean]
    end

    defmodule AgeAt do
      @moduledoc false
      use Ash.Query.Function, name: :age_at

      def args, do: [[:utc_datetime]]
      def returns, do: [:integer]
    end

    defmodule EngagementTuple do
      @moduledoc false
      use Ash.Type

      @impl true
      def storage_type(_), do: :map

      @impl true
      def composite?(_), do: true

      @impl true
      def acts_as(_), do: :tuple

      @impl true
      def cast_input(value, _), do: {:ok, value}

      @impl true
      def cast_stored(value, _), do: {:ok, value}

      @impl true
      def dump_to_native(value, _), do: {:ok, value}
    end

    defp ref(attribute) do
      %Ash.Query.Ref{
        attribute: Ash.Resource.Info.attribute(Post, attribute),
        relationship_path: [],
        resource: Post
      }
    end

    test "a ci_string attribute is accepted where :string is declared" do
      assert {:ok, %Shout{}} = Ash.Query.Function.new(Shout, [ref(:category), "x"])
    end

    test "a utc_datetime_usec attribute is accepted where :utc_datetime is declared" do
      assert {:ok, %AgeAt{}} = Ash.Query.Function.new(AgeAt, [ref(:inserted_at)])
    end

    test "a composite attribute is accepted where :map is declared" do
      assert {:ok, %Ash.Query.Function.GetPath{}} =
               Ash.Query.Function.new(Ash.Query.Function.GetPath, [ref(:engagement), [:likes]])
    end

    test "a composite type, and an array of one, act as a map" do
      assert Ash.Type.acts_as(Engagement) == :map
      assert Ash.Type.acts_as({:array, Engagement}) == {:array, :map}
    end

    test "a composite type that declares acts_as keeps it" do
      assert Ash.Type.acts_as(EngagementTuple) == :tuple
    end

    test "an integer attribute is still rejected where :string is declared" do
      assert {:error, message} = Ash.Query.Function.new(Shout, [ref(:score), "x"])
      assert message =~ "shout"
    end
  end

  describe "predicate function arguments that cannot be cast" do
    test "returns an invalid filter error rather than raising" do
      query = Ash.Query.filter_input(Post, %{"title" => %{"contains" => %{"a" => 1}}})

      assert [%Ash.Error.Query.InvalidFilterValue{}] = query.errors
      assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
    end
  end

  describe "boolean filter group input" do
    for operator <- ["and", "or"] do
      test "rejects malformed #{operator} values" do
        for value <- [[], %{}, "not a list"] do
          query =
            Ash.Query.filter_input(Post, %{
              unquote(operator) => value
            })

          assert [%Ash.Error.Query.InvalidFilterValue{}] = query.errors
          assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
        end
      end
    end
  end
end
