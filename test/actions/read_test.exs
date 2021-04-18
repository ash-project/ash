defmodule Ash.Test.Actions.ReadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset

  require Ash.Query

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Actions.ReadTest.Post, destination_field: :author1_id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    identities do
      identity :backup_id, [:uuid]
    end

    ets do
      private? true
    end

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :uuid, :uuid, default: &Ash.UUID.generate/0
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
      resource Post
      resource Author
    end
  end

  describe "api.get/3" do
    setup do
      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, post.id)

      assert fetched_post == post
    end

    test "it returns nil when there is no matching record" do
      assert {:ok, nil} = Api.get(Post, Ash.UUID.generate())
    end

    test "it uses identities if they exist", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, uuid: post.uuid)

      assert fetched_post == post
    end
  end

  describe "api.get!/3" do
    setup do
      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert ^post = Api.get!(Post, post.id)
    end

    test "it raises on an error", %{post: post} do
      assert_raise(Ash.Error.Invalid.NoSuchResource, ~r/\No such resource Something/, fn ->
        Api.get!(Something, post.id)
      end)
    end
  end

  describe "Api.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, []} = Api.read(Post)
    end
  end

  describe "Api.read!/2 with no records" do
    test "returns an empty result" do
      assert [] = Api.read!(Post)
    end
  end

  describe "Api.read/2" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet2"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "with a limit of 1, returns only 1 record" do
      assert {:ok, [_post]} =
               Post
               |> Ash.Query.limit(1)
               |> Api.read()
    end

    test "with a limit size of 2, returns 2 records" do
      assert {:ok, [_, _]} =
               Post
               |> Ash.Query.limit(2)
               |> Api.read()
    end

    test "with a limit of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, [_]} =
               Post
               |> Ash.Query.limit(1)
               |> Ash.Query.offset(1)
               |> Api.read()
    end
  end

  describe "Api.read!/2" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet2"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "it returns the records not in a tuple" do
      assert [_, _] = Api.read!(Post)
    end
  end

  describe "filters" do
    setup do
      post1 =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "test1", contents: "yeet"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "a filter that matches nothing returns no results" do
      assert {:ok, []} =
               Post
               |> Ash.Query.filter(contents == "not_yeet")
               |> Api.read()
    end

    test "a filter returns only matching records", %{post1: post1} do
      assert {:ok, [^post1]} =
               Post
               |> Ash.Query.filter(title == ^post1.title)
               |> Api.read()
    end

    test "a filter returns multiple records if they match", %{post1: post1, post2: post2} do
      assert {:ok, [_, _] = results} =
               Post
               |> Ash.Query.filter(contents == "yeet")
               |> Api.read()

      assert post1 in results
      assert post2 in results
    end
  end

  describe "select" do
    test "it automatically selects all fields" do
      author =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      assert author.name
      assert author.id
    end

    test "you can deselect a field" do
      Author
      |> new(%{name: "bruh"})
      |> Api.create!()

      assert [%{name: "bruh"}] = Api.read!(Author)
      assert [%{name: nil}] = Api.read!(Ash.Query.deselect(Author, :name))
    end

    test "you can select fields, but the primary key is always present" do
      Author
      |> new(%{name: "bruh"})
      |> Api.create!()

      assert [%{name: "bruh", id: id}] = Api.read!(Ash.Query.select(Author, :name))
      assert id
    end
  end

  describe "relationship filters" do
    setup do
      author1 =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      author2 =
        Author
        |> new(%{name: "bruh"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "test", contents: "yeet"})
        |> replace_relationship(:author1, author1)
        |> replace_relationship(:author2, author2)
        |> Api.create!()

      %{post: post, author1: author1, author2: author2}
    end

    test "you can filter on a related value", %{author1: author1} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id)
               |> Api.read!()
    end

    test "you can filter on multiple related values", %{author1: author1, author2: author2} do
      assert [_] =
               Post
               |> Ash.Query.filter(author1: author1.id, author2: author2.id)
               |> Api.read!()
    end
  end

  describe "sort" do
    setup do
      post1 =
        Post
        |> new(%{title: "abc", contents: "abc"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "xyz", contents: "abc"})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "a sort will sort the rows accordingly when ascending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post1, ^post2]} =
               Post
               |> Ash.Query.sort(title: :asc)
               |> Api.read()
    end

    test "a sort will sor rows accordingly when descending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, [^post2, ^post1]} =
               Post
               |> Ash.Query.sort(title: :desc)
               |> Api.read()
    end

    test "a nested sort sorts accordingly", %{post1: post1, post2: post2} do
      middle_post =
        Post
        |> new(%{title: "abc", contents: "xyz"})
        |> Api.create!()

      assert {:ok, [^post1, ^middle_post, ^post2]} =
               Post
               |> Ash.Query.sort(title: :asc, contents: :asc)
               |> Api.read()
    end
  end
end
