defmodule Ash.Test.Actions.SideLoadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Author do
    @moduledoc false
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Actions.SideLoadTest.Post,
        reverse_relationship: :author,
        destination_field: :author_id
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

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
      belongs_to :author, Author, reverse_relationship: :posts
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources [Author, Post]
  end

  describe "side_loads" do
    setup do
      author = Api.create!(Author, attributes: %{name: "zerg"})

      post1 =
        Api.create!(Post, attributes: %{title: "post1"}, relationships: %{author: author.id})

      post2 =
        Api.create!(Post, attributes: %{title: "post2"}, relationships: %{author: author.id})

      %{post1: post1, post2: post2}
    end

    test "it allows sideloading related data", %{post1: post1, post2: post2} do
      [author] =
        Author
        |> Api.query()
        |> Ash.Query.side_load(posts: [:author])
        |> Ash.Query.filter(posts: [id: post1.id])
        |> Api.read!()

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end
  end
end
