defmodule Ash.Test.Actions.Read do
  use ExUnit.Case, async: true

  alias Ash.Test.Post

  describe "Ash.get/3" do
    setup do
      {:ok, post} = Ash.create(Post, %{attributes: %{title: "test", contents: "yeet"}})
      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Ash.get(Post, post.id)

      assert fetched_post == post
    end

    test "it returns nil when there is no matching record" do
      assert {:ok, nil} = Ash.get(Post, Ash.UUID.generate())
    end
  end

  describe "Ash.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, %{results: []}} = Ash.read(Post)
    end
  end

  describe "Ash.read/2" do
    setup do
      {:ok, post1} = Ash.create(Post, %{attributes: %{title: "test", contents: "yeet"}})
      {:ok, post2} = Ash.create(Post, %{attributes: %{title: "test1", contents: "yeet2"}})

      %{post1: post1, post2: post2}
    end

    test "with page size of 1, returns only 1 record" do
      assert {:ok, %{results: [_post]}} = Ash.read(Post, %{page: %{limit: 1}})
    end

    test "with page size of 2, returns 2 records" do
      assert {:ok, %{results: [_, _]}} = Ash.read(Post, %{page: %{limit: 2}})
    end

    test "with page size of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, %{results: [_]}} = Ash.read(Post, %{page: %{limit: 1, offset: 1}})
    end
  end
end
