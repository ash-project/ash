# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Sort.SortTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
      attribute :private_name, :string
    end

    calculations do
      calculate :name_and_private_name, :string, expr(name <> " " <> private_name) do
        public? true
      end
    end

    actions do
      defaults [:read, :create, :update]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update]
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public? true
      end

      attribute :contents, :string do
        public? true
      end

      attribute :points, :integer
    end

    relationships do
      belongs_to :author, Author do
        public? true
      end

      belongs_to :private_author, Author

      has_many :comments_by_author, Ash.Test.Sort.SortTest.Comment do
        no_attributes? true
        public? true
        filter expr(author_id == parent(author.id))
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, :create, :update]
    end

    attributes do
      uuid_primary_key :id

      attribute :contents, :string do
        public? true
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Sort.SortTest.Author do
        public? true
      end

      belongs_to :post, Ash.Test.Sort.SortTest.Author do
        public? true
      end
    end
  end

  describe "sort input" do
    test "simple string sort parses properly" do
      assert %{sort: [title: :asc, contents: :desc]} =
               Ash.Query.sort_input(Post, "+title,-contents")
    end

    test "simple atom sort parses properly" do
      assert %{sort: [title: :asc]} =
               Ash.Query.sort_input(Post, :title)
    end

    test "a list of string sorts parse properly" do
      assert %{sort: [title: :asc, contents: :desc]} =
               Ash.Query.sort_input(Post, ["+title", "-contents"])
    end
  end

  describe "parse_input/2" do
    test "simple string sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, "+title,-contents")
    end

    test "a string sort can parse relationships" do
      {:ok, [{%Ash.Query.Calculation{}, :asc}] = sort} =
        Ash.Sort.parse_input(Post, "+author.name_and_private_name")

      Post
      |> Ash.Query.sort(sort)
      |> Ash.read!()
    end

    test "a string sort can parse calculations" do
      {:ok, [{%Ash.Query.Calculation{}, :desc_nils_last}]} =
        Ash.Sort.parse_input(Author, [{"name_and_private_name", {%{}, :desc_nils_last}}])

      assert [{%Ash.Query.Calculation{}, :desc_nils_last}] =
               Author
               |> Ash.Query.sort_input([{"name_and_private_name", {%{}, :desc_nils_last}}])
               |> Map.get(:sort)
    end

    test "a string sort can parse relationships as atoms" do
      {:ok, [{%Ash.Query.Calculation{}, :asc}] = sort} =
        Ash.Sort.parse_input(Post, "author.name_and_private_name": :asc)

      Post
      |> Ash.Query.sort(sort)
      |> Ash.read!()
    end

    test "a sort can be just a string" do
      {:ok, [{%Ash.Query.Calculation{}, :asc}] = sort} =
        Ash.Sort.parse_input(Post, "author.name_and_private_name")

      Post
      |> Ash.Query.sort(sort)
      |> Ash.read!()
    end

    test "a string sort honors private relationships" do
      {:error,
       %Ash.Error.Query.NoSuchField{
         resource: Ash.Test.Sort.SortTest.Post,
         field: "name"
       }} =
        Ash.Sort.parse_input(Post, "+private_author.name")
    end

    test "a string sort honors private fields" do
      {:error,
       %Ash.Error.Query.NoSuchField{
         resource: Ash.Test.Sort.SortTest.Author,
         field: "private_name"
       }} =
        Ash.Sort.parse_input(Post, "+author.private_name")
    end

    test "private attributes cannot be used" do
      assert {:error, %Ash.Error.Query.NoSuchField{}} = Ash.Sort.parse_input(Post, "points")
    end

    test "a list sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, ["title", "-contents"])
    end

    test "a regular sort parses properly" do
      assert {:ok, [title: :asc, contents: :desc]} =
               Ash.Sort.parse_input(Post, title: :asc, contents: :desc)
    end

    test "++ and -- modifiers work properly" do
      assert {:ok, [title: :asc_nils_first, contents: :desc_nils_last]} =
               Ash.Sort.parse_input(Post, "++title,--contents")
    end

    test "can sort by nested relationships" do
      author = Author |> Ash.Changeset.for_create(:create) |> Ash.create!()
      author2 = Author |> Ash.Changeset.for_create(:create) |> Ash.create!()
      Post |> Ash.Changeset.for_create(:create, %{author_id: author.id}) |> Ash.create!()
      Post |> Ash.Changeset.for_create(:create, %{author_id: author2.id}) |> Ash.create!()

      sort = Ash.Sort.parse_input!(Post, "-author.id")
      author_ids = Post |> Ash.Query.sort(sort) |> Ash.read!() |> Enum.map(& &1.author_id)
      assert Enum.reverse(Enum.sort([author.id, author2.id])) == author_ids

      sort = Ash.Sort.parse_input!(Post, "author.id")
      author_ids = Post |> Ash.Query.sort(sort) |> Ash.read!() |> Enum.map(& &1.author_id)
      assert Enum.sort([author.id, author2.id]) == author_ids
      # TODO: Write assertion here
    end
  end
end
