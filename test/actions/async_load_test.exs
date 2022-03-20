defmodule Ash.Test.Actions.AsyncLoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  import Ash.Changeset
  require Ash.Query

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [
        Ash.Test.Authorizer
      ]

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_many :posts, Ash.Test.Actions.AsyncLoadTest.Post, destination_field: :author_id

      has_one :latest_post, Ash.Test.Actions.AsyncLoadTest.Post,
        destination_field: :author_id,
        sort: [inserted_at: :desc]
    end
  end

  defmodule PostsInSameCategory do
    use Ash.Resource.ManualRelationship

    def load(posts, _, %{query: destination_query, api: api}) do
      categories = Enum.map(posts, & &1.category)

      other_posts =
        destination_query
        |> Ash.Query.filter(category in ^categories)
        |> api.read!()
        |> Enum.group_by(& &1.category)

      {:ok,
       Map.new(posts, fn post ->
         related_posts =
           other_posts
           |> Map.get(post.category, [])
           |> Enum.reject(&(&1.id == post.id))

         {Map.take(post, [:id]), related_posts}
       end)}
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      read :read
      create :create
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :category, :string
      timestamps()
    end

    relationships do
      belongs_to :author, Author

      has_many :posts_in_same_category, __MODULE__ do
        manual PostsInSameCategory
      end

      many_to_many :categories, Ash.Test.Actions.AsyncLoadTest.Category,
        through: Ash.Test.Actions.AsyncLoadTest.PostCategory,
        destination_field_on_join_table: :category_id,
        source_field_on_join_table: :post_id
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      read :read
      create :create
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, required?: true

      belongs_to :category, Ash.Test.Actions.AsyncLoadTest.Category,
        primary_key?: true,
        required?: true
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

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
    capture_log(fn ->
      Ash.DataLayer.Mnesia.start(Api)
    end)

    on_exit(fn ->
      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)

    start_supervised(
      {Ash.Test.Authorizer,
       strict_check: :authorized,
       check: {:error, Ash.Error.Forbidden.exception([])},
       strict_check_context: [:query]}
    )

    :ok
  end

  describe "loads" do
    test "it allows loading manual relationships" do
      post1 =
        Post
        |> new(%{title: "post1", category: "foo"})
        |> Api.create!()

      Post
      |> new(%{title: "post2", category: "bar"})
      |> Api.create!()

      post3 =
        Post
        |> new(%{title: "post2", category: "foo"})
        |> Api.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Api.load!(:posts_in_same_category)
               |> Map.get(:posts_in_same_category)
    end

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
