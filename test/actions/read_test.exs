defmodule Ash.Test.Actions.ReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Actions.ReadTest.Post, destination_field: :author
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author1, Ash.Test.Actions.ReadTest.Author
      belongs_to :author2, Ash.Test.Actions.ReadTest.Author
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Post)
      resource(Author)
    end
  end

  describe "api.get/3" do
    setup do
      {:ok, post} = Api.create(Post, attributes: %{title: "test", contents: "yeet"})
      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, post.id)

      assert fetched_post == post
    end

    test "it returns nil when there is no matching record" do
      assert {:ok, nil} = Api.get(Post, Ecto.UUID.generate())
    end
  end

  describe "api.get!/3" do
    setup do
      {:ok, post} = Api.create(Post, attributes: %{title: "test", contents: "yeet"})
      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert ^post = Api.get!(Post, post.id)
    end

    test "it raises on an error", %{post: post} do
      assert_raise(Ash.Error.Invalid, ~r/\* No such resource Something/, fn ->
        Api.get!(Something, post.id)
      end)
    end
  end

  describe "api.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, []} = Api.read(Post)
    end
  end

  describe "Ash.read!/2 with no records" do
    test "returns an empty result" do
      assert [] = Api.read!(Post)
    end
  end

  describe "api.read/2" do
    setup do
      {:ok, post1} = Api.create(Post, attributes: %{title: "test", contents: "yeet"})
      {:ok, post2} = Api.create(Post, attributes: %{title: "test1", contents: "yeet2"})

      %{post1: post1, post2: post2}
    end

    test "with a limit of 1, returns only 1 record" do
      assert {:ok, [_post]} =
               Post
               |> Api.query()
               |> Ash.Query.limit(1)
               |> Api.read()
    end

    test "with a limit size of 2, returns 2 records" do
      assert {:ok, [_, _]} =
               Post
               |> Api.query()
               |> Ash.Query.limit(2)
               |> Api.read()
    end

    test "with a limit of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, [_]} =
               Post
               |> Api.query()
               |> Ash.Query.limit(1)
               |> Ash.Query.offset(1)
               |> Api.read()
    end
  end

  describe "api.read!/2" do
    setup do
      {:ok, post1} = Api.create(Post, attributes: %{title: "test", contents: "yeet"})
      {:ok, post2} = Api.create(Post, attributes: %{title: "test1", contents: "yeet2"})

      %{post1: post1, post2: post2}
    end

    test "it returns the records not in a tuple" do
      assert [_, _] = Api.read!(Post)
    end

    test "it raises on an error" do
      assert_raise(
        Ash.Error.Invalid,
        ~r/Invalid filter value `10` supplied in: `title == 10`/,
        fn ->
          Post
          |> Api.query()
          |> Ash.Query.filter(title: 10)
          |> Api.read!()
        end
      )
    end
  end

  describe "filters" do
    setup do
      {:ok, post1} = Api.create(Post, attributes: %{title: "test", contents: "yeet"})
      {:ok, post2} = Api.create(Post, attributes: %{title: "test1", contents: "yeet"})

      %{post1: post1, post2: post2}
    end

    test "a filter that matches nothing returns no results" do
      assert {:ok, []} =
               Post
               |> Api.query()
               |> Ash.Query.filter(contents: "not_yeet")
               |> Api.read()
    end

    test "a filter returns only matching records", %{post1: post1} do
      assert {:ok, [^post1]} =
               Post
               |> Api.query()
               |> Ash.Query.filter(title: post1.title)
               |> Api.read()
    end

    test "a filter returns multiple records if they match", %{post1: post1, post2: post2} do
      assert {:ok, [_, _] = results} =
               Post
               |> Api.query()
               |> Ash.Query.filter(contents: "yeet")
               |> Api.read()

      assert post1 in results
      assert post2 in results
    end
  end

  describe "relationship filters" do
    setup do
      author1 = Api.create!(Author, attributes: %{name: "bruh"})
      author2 = Api.create!(Author, attributes: %{name: "bruh"})

      {:ok, post} =
        Api.create(Post,
          attributes: %{title: "test", contents: "yeet"},
          relationships: %{author1: author1.id, author2: author2.id}
        )

      %{post: post, author1: author1, author2: author2}
    end

    test "you can filter on a related value", %{author1: author1} do
      assert [_] =
               Post
               |> Api.query()
               |> Ash.Query.filter(author1: author1.id)
               |> Api.read!()
    end

    test "you can filter on multiple related values", %{author1: author1, author2: author2} do
      assert [_] =
               Post
               |> Api.query()
               |> Ash.Query.filter(author1: author1.id, author2: author2.id)
               |> Api.read!()
    end
  end

  describe "sort" do
    setup do
      {:ok, post1} = Api.create(Post, attributes: %{title: "abc", contents: "abc"})
      {:ok, post2} = Api.create(Post, attributes: %{title: "xyz", contents: "abc"})

      %{post1: post1, post2: post2}
    end

    test "a sort will sort the rows accordingly when ascending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post1, ^post2]} =
               Post
               |> Api.query()
               |> Ash.Query.sort(title: :asc)
               |> Api.read()
    end

    test "a sort will sor rows accordingly when descending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post2, ^post1]} =
               Post
               |> Api.query()
               |> Ash.Query.sort(title: :desc)
               |> Api.read()
    end

    test "a nested sort sorts accordingly", %{post1: post1, post2: post2} do
      {:ok, middle_post} = Api.create(Post, attributes: %{title: "abc", contents: "xyz"})

      assert {:ok, [^post1, ^middle_post, ^post2]} =
               Post
               |> Api.query()
               |> Ash.Query.sort(title: :asc, contents: :asc)
               |> Api.read()
    end
  end
end
