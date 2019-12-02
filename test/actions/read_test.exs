defmodule Ash.Test.Actions.ReadTest do
  use ExUnit.Case, async: true
  # import Ash.Test

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      defaults [:read, :create]
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
    end
  end

  defmodule Api do
    use Ash.Api

    api do
      resources [Post]
    end
  end

  describe "api.get/3" do
    setup do
      {:ok, post} = Api.create(Post, %{attributes: %{title: "test", contents: "yeet"}})
      %{post: post}
    end

    test "it returns a matching record", %{post: post} do
      assert {:ok, fetched_post} = Api.get(Post, post.id)

      assert fetched_post == post
    end

    test "it returns nil when there is no matching record" do
      assert {:ok, nil} = Api.get(Post, Ash.UUID.generate())
    end
  end

  describe "Ash.read/2 with no records" do
    test "returns an empty result" do
      assert {:ok, %{results: []}} = Api.read(Post)
    end
  end

  describe "Ash.read/2" do
    setup do
      {:ok, post1} = Api.create(Post, %{attributes: %{title: "test", contents: "yeet"}})
      {:ok, post2} = Api.create(Post, %{attributes: %{title: "test1", contents: "yeet2"}})

      %{post1: post1, post2: post2}
    end

    test "with page size of 1, returns only 1 record" do
      assert {:ok, %{results: [_post]}} = Api.read(Post, %{page: %{limit: 1}})
    end

    test "with page size of 2, returns 2 records" do
      assert {:ok, %{results: [_, _]}} = Api.read(Post, %{page: %{limit: 2}})
    end

    test "with page size of 1 and an offset of 1, it returns 1 record" do
      assert {:ok, %{results: [_]}} = Api.read(Post, %{page: %{limit: 1, offset: 1}})
    end
  end

  describe "filters" do
    setup do
      {:ok, post1} = Api.create(Post, %{attributes: %{title: "test", contents: "yeet"}})
      {:ok, post2} = Api.create(Post, %{attributes: %{title: "test1", contents: "yeet"}})

      %{post1: post1, post2: post2}
    end

    test "a filter that matches nothing returns no results" do
      assert {:ok, %{results: []}} = Api.read(Post, %{filter: %{contents: "not_yeet"}})
    end

    test "a filter returns only matching records", %{post1: post1} do
      assert {:ok, %{results: [^post1]}} = Api.read(Post, %{filter: %{title: post1.title}})
    end

    test "a filter returns multiple records if they match", %{post1: post1, post2: post2} do
      assert {:ok, %{results: [_, _] = results}} = Api.read(Post, %{filter: %{contents: "yeet"}})

      assert post1 in results
      assert post2 in results
    end
  end

  describe "sort" do
    setup do
      {:ok, post1} = Api.create(Post, %{attributes: %{title: "abc", contents: "abc"}})
      {:ok, post2} = Api.create(Post, %{attributes: %{title: "xyz", contents: "abc"}})

      %{post1: post1, post2: post2}
    end

    test "a sort will sort the rows accordingly when ascending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, %{results: [^post1, ^post2]}} = Api.read(Post, %{sort: [asc: :title]})
    end

    test "a sort will sor rows accordingly when descending", %{
      post1: post1,
      post2: post2
    } do
      assert {:ok, %{results: [^post2, ^post1]}} = Api.read(Post, %{sort: [desc: :title]})
    end

    test "a nested sort sorts accordingly", %{post1: post1, post2: post2} do
      {:ok, middle_post} = Api.create(Post, %{attributes: %{title: "abc", contents: "xyz"}})

      assert {:ok, %{results: [^post1, ^middle_post, ^post2]}} =
               Api.read(Post, %{sort: [asc: :title, asc: :contents]})
    end
  end
end
