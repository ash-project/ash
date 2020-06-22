defmodule Ash.Test.Actions.SideLoadTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [
        Ash.Test.Authorizer
      ]

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
      has_many :posts, Ash.Test.Actions.SideLoadTest.Post,
        reverse_relationship: :author,
        destination_field: :author_id
    end
  end

  outer_mod = __MODULE__

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
      belongs_to :author, Author, reverse_relationship: :posts

      many_to_many :categories, Module.concat(outer_mod, Category),
        through: Module.concat(outer_mod, PostCategory),
        destination_field_on_join_table: :category_id,
        source_field_on_join_table: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
    end

    relationships do
      belongs_to :post, Post, primary_key?: true
      belongs_to :category, Module.concat(outer_mod, Category), primary_key?: true
    end
  end

  defmodule Category do
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
      many_to_many :posts, Post,
        through: PostCategory,
        destination_field_on_join_table: :post_id,
        source_field_on_join_table: :category_id
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Author)
      resource(Post)
      resource(Category)
      resource(PostCategory)
    end
  end

  setup do
    Process.put(:authorize?, true)
    Process.put(:strict_check_context, [:query])

    :ok
  end

  describe "side_loads" do
    test "it allows sideloading related data" do
      author = Api.create!(Author, attributes: %{name: "zerg"})

      post1 =
        Api.create!(Post, attributes: %{title: "post1"}, relationships: %{author: author.id})

      post2 =
        Api.create!(Post, attributes: %{title: "post2"}, relationships: %{author: author.id})

      [author] =
        Author
        |> Api.query()
        |> Ash.Query.side_load(posts: [:author])
        |> Ash.Query.filter(posts: [id: post1.id])
        |> Api.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "it allows sideloading many to many relationships" do
      category1 = Api.create!(Category, attributes: %{name: "lame"})
      category2 = Api.create!(Category, attributes: %{name: "cool"})

      post =
        Api.create!(Post,
          attributes: %{title: "post1"},
          relationships: %{categories: [category1, category2]}
        )

      [post] =
        Post
        |> Api.query()
        |> Ash.Query.side_load(:categories)
        |> Ash.Query.filter(id: post.id)
        |> Api.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it allows sideloading nested many to many relationships" do
      category1 = Api.create!(Category, attributes: %{name: "lame"})
      category2 = Api.create!(Category, attributes: %{name: "cool"})

      post =
        Api.create!(Post,
          attributes: %{title: "post1"},
          relationships: %{categories: [category1, category2]}
        )

      [post] =
        Post
        |> Api.query()
        |> Ash.Query.side_load(categories: :posts)
        |> Ash.Query.filter(id: post.id)
        |> Api.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end
  end
end
