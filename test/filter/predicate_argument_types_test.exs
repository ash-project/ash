# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Filter.PredicateArgumentTypesTest do
  @moduledoc """
  A predicate function applied to an attribute whose type cannot satisfy the
  predicate's declared argument types is an invalid filter, not something to
  hand to the data layer (ash-project/ash#2939).
  """
  use ExUnit.Case, async: true

  alias Ash.Error.Query.InvalidFilterValue
  alias Ash.Test.Domain, as: Domain

  require Ash.Query

  defmodule Slug do
    use Ash.Type.NewType, subtype_of: :string, constraints: [match: ~r/^[a-z]+$/]
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :slug, Slug, public?: true
      attribute :ci_title, :ci_string, public?: true
      attribute :status, :atom, public?: true, constraints: [one_of: [:draft, :published]]
      attribute :score, :integer, public?: true
      attribute :tags, {:array, :string}, public?: true

      attribute :valid_at, :range,
        public?: true,
        constraints: [inner_type: :integer]
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end
  end

  defp range(lower, upper), do: %Ash.Range{lower: lower, upper: upper, bounds: :"[)"}

  setup do
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "one",
        slug: "one",
        ci_title: "One",
        status: :published,
        score: 1,
        tags: ["a", "b"],
        valid_at: range(1, 10)
      })
      |> Ash.create!()

    %{post: post}
  end

  describe "filter_input/2 with a predicate on an attribute of the wrong type" do
    for predicate <- ["contains", "string_starts_with", "string_ends_with"] do
      test "#{predicate} on an integer attribute is an invalid filter" do
        query = Ash.Query.filter_input(Post, %{"score" => %{unquote(predicate) => "x"}})

        refute query.valid?
        assert [%InvalidFilterValue{path: [:filter]} = error] = query.errors
        assert Exception.message(error) =~ "Could not cast function arguments"
        assert Exception.message(error) =~ "score is of type Ash.Type.Integer"
        assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
      end
    end

    for predicate <- ["range_overlaps", "range_adjacent", "range_contains"] do
      test "#{predicate} on a string attribute is an invalid filter" do
        query = Ash.Query.filter_input(Post, %{"title" => %{unquote(predicate) => "x"}})

        refute query.valid?
        assert [%InvalidFilterValue{path: [:filter]} = error] = query.errors
        assert Exception.message(error) =~ "requires range arguments"
        assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
      end
    end

    test "contains on a list attribute is an invalid filter" do
      query = Ash.Query.filter_input(Post, %{"tags" => %{"contains" => "a"}})

      refute query.valid?
      assert [%InvalidFilterValue{}] = query.errors
    end

    test "an uncastable literal is reported as an invalid filter instead of raising" do
      query = Ash.Query.filter_input(Post, %{"title" => %{"contains" => %{"not" => "a string"}}})

      refute query.valid?
      assert [%InvalidFilterValue{path: [:filter]}] = query.errors
    end
  end

  describe "filter_input/2 with a predicate on a compatible attribute" do
    test "a string attribute", %{post: post} do
      query = Ash.Query.filter_input(Post, %{"title" => %{"contains" => "on"}})
      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id
    end

    test "a NewType of string", %{post: post} do
      query = Ash.Query.filter_input(Post, %{"slug" => %{"string_starts_with" => "o"}})
      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id
    end

    test "a ci_string attribute", %{post: post} do
      query = Ash.Query.filter_input(Post, %{"ci_title" => %{"contains" => "ONE"}})
      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id
    end

    test "an atom attribute, which is stored as a string", %{post: post} do
      query = Ash.Query.filter_input(Post, %{"status" => %{"string_starts_with" => "pub"}})
      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id
    end

    test "a range attribute with the range predicates", %{post: post} do
      query = Ash.Query.filter_input(Post, %{"valid_at" => %{"range_overlaps" => range(5, 15)}})
      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id

      query = Ash.Query.filter_input(Post, %{"valid_at" => %{"range_contains" => range(2, 3)}})
      assert query.valid?
      assert [_] = Ash.read!(query)

      query = Ash.Query.filter_input(Post, %{"valid_at" => %{"range_adjacent" => range(10, 20)}})
      assert query.valid?
      assert [_] = Ash.read!(query)

      query = Ash.Query.filter_input(Post, %{"valid_at" => %{"range_overlaps" => range(50, 60)}})
      assert query.valid?
      assert [] = Ash.read!(query)
    end
  end

  describe "Ash.Query.filter/2 expressions" do
    test "a predicate on an attribute of the wrong type is an invalid filter" do
      query = Ash.Query.filter(Post, contains(score, "x"))

      refute query.valid?
      assert [%InvalidFilterValue{}] = query.errors

      query = Ash.Query.filter(Post, range_overlaps(title, "x"))

      refute query.valid?
      assert [%InvalidFilterValue{}] = query.errors
    end

    test "two references of the right type still work", %{post: post} do
      query = Ash.Query.filter(Post, range_overlaps(valid_at, valid_at) and contains(title, slug))

      assert query.valid?
      assert [%{id: id}] = Ash.read!(query)
      assert id == post.id
    end

    test "non-predicate functions are not affected" do
      # `string_length/1` declares `[:string]`; today a reference of another
      # type is left to the data layer, and that stays as it is.
      assert Ash.Query.filter(Post, string_length(score) > 0).valid?
    end
  end

  describe "Ash.Query.Function.try_cast_arguments/3" do
    test "only checks references when asked" do
      score = %Ash.Query.Ref{
        attribute: Ash.Resource.Info.attribute(Post, :score),
        relationship_path: [],
        resource: Post
      }

      assert [^score, "x"] =
               Ash.Query.Function.try_cast_arguments([:string, :string], [score, "x"])

      assert nil ==
               Ash.Query.Function.try_cast_arguments([:string, :string], [score, "x"],
                 check_refs?: true
               )

      assert [^score, "x"] =
               Ash.Query.Function.try_cast_arguments([:any, :same], [score, "x"],
                 check_refs?: true
               )
    end
  end
end
