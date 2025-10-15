# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.DefaultSortTest do
  use ExUnit.Case, async: true

  defmodule Post do
    use Ash.Resource, domain: Ash.Test.Query.DefaultSortTest.Domain

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

  defmodule SortPreparation do
    use Ash.Resource.Preparation

    def init(opts) do
      {:ok, opts}
    end

    def prepare(query, opts, _context) do
      Ash.Query.default_sort(query, opts[:sort])
    end
  end

  defmodule PostWithPreparation do
    use Ash.Resource, domain: Ash.Test.Query.DefaultSortTest.Domain

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :published_at, :utc_datetime
    end

    preparations do
      prepare {SortPreparation, sort: [published_at: :desc]}
    end

    actions do
      defaults [:read, :create]
    end
  end

  defmodule Author do
    use Ash.Resource, domain: Ash.Test.Query.DefaultSortTest.Domain

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Post
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
      resource Author
      resource PostWithPreparation
    end
  end

  alias Ash.Test.Query.DefaultSortTest.{Author, Comment, Post, PostWithPreparation}

  describe "default_sort/2" do
    test "applies sort when no sort is specified" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.default_sort(:title)

      assert query.sort == [{:title, :asc}]
    end

    test "does not apply sort when a sort is already specified" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.sort(:published_at)
        |> Ash.Query.default_sort(:title)

      assert query.sort == [{:published_at, :asc}]
    end

    test "supports sort direction" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.default_sort(title: :desc)

      assert query.sort == [{:title, :desc}]
    end

    test "supports multiple sort fields" do
      query =
        Post
        |> Ash.Query.new()
        |> Ash.Query.default_sort(title: :asc, published_at: :desc)

      assert query.sort == [{:title, :asc}, {:published_at, :desc}]
    end
  end

  describe "build/2 with default_sort" do
    test "applies default_sort through build" do
      query = Ash.Query.build(Post, default_sort: :title)
      assert query.sort == [{:title, :asc}]
    end

    test "default_sort doesn't override existing sort" do
      query = Ash.Query.build(Post, sort: :published_at, default_sort: :title)
      assert query.sort == [{:published_at, :asc}]
    end
  end

  describe "preparation with default_sort" do
    test "applies default sort from preparation" do
      query =
        PostWithPreparation
        |> Ash.Query.for_read(:read)

      assert query.sort == [{:published_at, :desc}]
    end

    test "preparation default_sort doesn't override explicit sort" do
      query =
        PostWithPreparation
        |> Ash.Query.new()
        |> Ash.Query.sort(:title)
        |> Ash.Query.for_read(:read)

      assert query.sort == [{:title, :asc}]
    end
  end
end
