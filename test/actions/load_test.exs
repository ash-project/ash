defmodule Ash.Test.Actions.LoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import Ash.Changeset
  require Ash.Query

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
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Actions.LoadTest.Post, destination_field: :author_id

      has_one :latest_post, Ash.Test.Actions.LoadTest.Post,
        destination_field: :author_id,
        sort: [inserted_at: :desc]
    end
  end

  defmodule Post do
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
      attribute :title, :string
      attribute :contents, :string
      timestamps()
    end

    relationships do
      belongs_to :author, Author

      many_to_many :categories, Ash.Test.Actions.LoadTest.Category,
        through: Ash.Test.Actions.LoadTest.PostCategory,
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
      read :read
      create :create
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, required?: true

      belongs_to :category, Ash.Test.Actions.LoadTest.Category,
        primary_key?: true,
        required?: true
    end
  end

  defmodule Category do
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
      many_to_many :posts, Post,
        through: PostCategory,
        destination_field_on_join_table: :post_id,
        source_field_on_join_table: :category_id
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
      entry(Post)
      entry(Category)
      entry(PostCategory)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  setup do
    start_supervised(
      {Ash.Test.Authorizer,
       strict_check: :authorized,
       check: {:error, Ash.Error.Forbidden.exception([])},
       strict_check_context: [:query]}
    )

    :ok
  end

  describe "loads" do
    test "it allows loading related data" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "post1"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "post2"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Api.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "it allows loading many to many relationships" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> replace_relationship(:categories, [category1, category2])
        |> Api.create!()

      [post] =
        Post
        |> Ash.Query.load(:categories)
        |> Ash.Query.filter(id == ^post.id)
        |> Api.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it allows loading nested many to many relationships" do
      category1 =
        Category
        |> new(%{name: "lame"})
        |> Api.create!()

      category2 =
        Category
        |> new(%{name: "cool"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "post1"})
        |> replace_relationship(:categories, [category1, category2])
        |> Api.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts)
        |> Ash.Query.filter(id == ^post.id)
        |> Api.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "it loads sorted relationships in the proper order" do
      author =
        Author
        |> new(%{name: "zerg"})
        |> Api.create!()

      _post1 =
        Post
        |> new(%{title: "post1"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      :timer.sleep(2)

      post2 =
        Post
        |> new(%{title: "post2"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      [author] =
        Author
        |> Ash.Query.load(:latest_post)
        |> Api.read!()

      assert author.latest_post.id == post2.id
    end
  end
end
