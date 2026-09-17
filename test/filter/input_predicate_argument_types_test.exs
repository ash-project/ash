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
      attribute(:score, :integer, public?: true)
      attribute(:tags, {:array, :string}, public?: true)

      attribute(:valid_at, Ash.Type.Range,
        public?: true,
        constraints: [inner_type: :integer]
      )
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
  end

  describe "predicate function arguments that cannot be cast" do
    test "returns an invalid filter error rather than raising" do
      query = Ash.Query.filter_input(Post, %{"title" => %{"contains" => %{"a" => 1}}})

      assert [%Ash.Error.Query.InvalidFilterValue{}] = query.errors
      assert {:error, %Ash.Error.Invalid{}} = Ash.read(query)
    end
  end
end
