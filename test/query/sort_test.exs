defmodule Ash.Test.Query.SortTest do
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource, domain: Ash.Test.Query.SortTest.Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :published_at, :utc_datetime
      attribute :author_id, :uuid
    end

    actions do
      defaults [:read, :create]
    end
  end

  defmodule Comment do
    use Ash.Resource, domain: Ash.Test.Query.DefaultSortTest.Domain

    attributes do
      uuid_primary_key :id
      attribute :body, :string
      attribute :created_at, :utc_datetime
    end

    relationships do
      belongs_to :post, Post
    end

    actions do
      defaults [:read, :create]
    end
  end

  defmodule Domain do
    use Ash.Domain

    resources do
      resource Post
      resource Comment
    end
  end

  alias Ash.Test.Query.SortTest.{Comment, Post}

  describe "sort/2" do
    test "applies :asc when nothing else is specified" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.sort(:published_at)

      assert query.sort == [{:published_at, :asc}]
    end

    test "works with string param" do
      query =
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.Query.sort("published_at")

      assert query.sort == [{:published_at, :asc}]
    end

    test "works with string param with negative prefix" do
      query =
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.Query.sort("-published_at")

      assert query.sort == [{:published_at, :desc}]
    end

    test "works with string param with double negative prefix" do
      query =
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.Query.sort("--published_at")

      assert query.sort == [{:published_at, :desc_nils_last}]
    end

    test "works with string param with positive prefix" do
      query =
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.Query.sort("+published_at")

      assert query.sort == [{:published_at, :asc}]
    end

    test "works with string param with double positive prefix" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.sort("++published_at")

      assert query.sort == [{:published_at, :asc_nils_first}]
    end

    test "handles string prefix with multiple fields" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.sort("title,++published_at,-author_id")

      assert query.sort == [{:title, :asc}, {:published_at, :asc_nils_first}, {:author_id, :desc}]
    end

    test "handles string prefix with dot notation" do
      query =
        Comment
        |> Ash.Query.new()
        |> Ash.Query.sort("--post.title,++post.published_at,post.author_id")

      assert {%Ash.Query.Calculation{module: Ash.Resource.Calculation.Expression},
              :desc_nils_last} =
               Enum.at(query.sort, 0)

      assert {%Ash.Query.Calculation{module: Ash.Resource.Calculation.Expression},
              :asc_nils_first} =
               Enum.at(query.sort, 1)

      assert {%Ash.Query.Calculation{module: Ash.Resource.Calculation.Expression}, :asc} =
               Enum.at(query.sort, 2)
    end
  end
end
