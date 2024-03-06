defmodule Ash.Test.Actions.AsyncLoadTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog
  require Ash.Query
  alias Ash.Test.Domain, as: Domain

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [
        Ash.Test.Authorizer
      ]

    actions do
      default_accept :*
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      has_many :posts, Ash.Test.Actions.AsyncLoadTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_many :authorized_actor_posts, Ash.Test.Actions.AsyncLoadTest.Post,
        destination_attribute: :author_id,
        read_action: :authorized_actor,
        public?: true

      has_many :authorized_context_posts, Ash.Test.Actions.AsyncLoadTest.Post,
        destination_attribute: :author_id,
        read_action: :authorized_context,
        public?: true

      has_one :latest_post, Ash.Test.Actions.AsyncLoadTest.Post,
        destination_attribute: :author_id,
        sort: [inserted_at: :desc],
        public?: true
    end
  end

  defmodule PostsInSameCategory do
    use Ash.Resource.ManualRelationship

    def select(_), do: [:category]

    def load(posts, _, %{query: destination_query, domain: domain}) do
      categories = Enum.map(posts, & &1.category)

      other_posts =
        destination_query
        |> Ash.Query.filter(category in ^categories)
        |> domain.read!()
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
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Mnesia,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:create, :read]

      read :authorized_actor
      read :authorized_context
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :category, :string do
        public?(true)
      end

      attribute :actor_id, :string do
        public?(true)
      end

      timestamps()
    end

    relationships do
      belongs_to :author, Author, public?: true

      has_many :posts_in_same_category, __MODULE__ do
        public?(true)
        manual PostsInSameCategory
      end

      many_to_many :categories, Ash.Test.Actions.AsyncLoadTest.Category,
        through: Ash.Test.Actions.AsyncLoadTest.PostCategory,
        destination_attribute_on_join_resource: :category_id,
        source_attribute_on_join_resource: :post_id,
        public?: true
    end

    calculations do
      calculate :title_plus_title, :string, expr((title || "foo") <> (title || "bar")) do
        public? true
      end
    end

    policies do
      policy action(:authorized_actor) do
        authorize_if expr(actor_id == ^actor(:id))
      end

      policy action(:authorized_context) do
        authorize_if context_equals(:authorized?, true)
      end

      policy always() do
        authorize_if always()
      end
    end
  end

  defmodule PostCategory do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    actions do
      default_accept :*
      defaults [:create, :read, :destroy]
    end

    relationships do
      belongs_to :post, Post, primary_key?: true, allow_nil?: false, public?: true

      belongs_to :category, Ash.Test.Actions.AsyncLoadTest.Category,
        public?: true,
        primary_key?: true,
        allow_nil?: false
    end
  end

  defmodule Category do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    actions do
      default_accept :*
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end

    relationships do
      many_to_many :posts, Post,
        public?: true,
        through: PostCategory,
        destination_attribute_on_join_resource: :post_id,
        source_attribute_on_join_resource: :category_id
    end
  end

  describe "context" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [Category, PostCategory, Post, Author])
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

    test "context provides the actor" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      Ash.set_actor(author)

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", actor_id: author.id})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      authorized_posts =
        author
        |> Domain.load!(:authorized_actor_posts)
        |> Map.get(:authorized_actor_posts)

      post1_id = post1.id
      assert [%{id: ^post1_id}] = authorized_posts
    end

    test "context provides the context" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      Ash.set_actor(nil)
      Ash.set_context(%{authorized?: false})

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post1", actor_id: author.id})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2"})
      |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
      |> Domain.create!()

      assert {:error, %Ash.Error.Forbidden{}} =
               author
               |> Domain.load(:authorized_context_posts)
    end
  end

  describe "loads" do
    setup do
      capture_log(fn ->
        Ash.DataLayer.Mnesia.start(Domain, [Category, PostCategory, Post, Author])
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

    test "it allows loading manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Domain.create!()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
        |> Domain.create!()

      post3_id = post3.id

      assert [%{id: ^post3_id}] =
               post1
               |> Domain.load!(:posts_in_same_category)
               |> Map.get(:posts_in_same_category)
    end

    test "it allows loading through manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
      |> Domain.create!()

      assert [%Post{title: "post1"}] =
               post1
               |> Domain.load!(posts_in_same_category: :posts_in_same_category)
               |> Map.get(:posts_in_same_category)
               |> Enum.flat_map(&Map.get(&1, :posts_in_same_category))
    end

    test "it allows loading calculations on and through manual relationships" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1", category: "foo"})
        |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "bar"})
      |> Domain.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "post2", category: "foo"})
      |> Domain.create!()

      assert %Post{
               title: "post1",
               title_plus_title: "post1post1",
               posts_in_same_category: [
                 %{
                   title: "post2",
                   title_plus_title: "post2post2",
                   posts_in_same_category: [
                     %{title: "post1", title_plus_title: "post1post1"}
                   ]
                 }
               ]
             } =
               post1
               |> Domain.load!([
                 :title_plus_title,
                 posts_in_same_category: [
                   :title_plus_title,
                   posts_in_same_category: [:title_plus_title]
                 ]
               ])
    end

    test "it allows loading related data" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(posts: [:author])
        |> Ash.Query.filter(posts.id == ^post1.id)
        |> Domain.read!(authorize?: true)

      assert Enum.sort(Enum.map(author.posts, &Map.get(&1, :id))) ==
               Enum.sort([post1.id, post2.id])

      for post <- author.posts do
        assert post.author.id == author.id
      end
    end

    test "it allows loading many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      [post] =
        Post
        |> Ash.Query.load(:categories)
        |> Ash.Query.filter(id == ^post.id)
        |> Domain.read!(authorize?: true)

      assert [%{id: id1}, %{id: id2}] = post.categories

      assert Enum.sort([category1.id, category2.id]) == Enum.sort([id1, id2])
    end

    test "it allows loading nested many to many relationships" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Domain.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts)
        |> Ash.Query.filter(id == ^post.id)
        |> Domain.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "loading multiple at once works" do
      category1 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "lame"})
        |> Domain.create!()

      category2 =
        Category
        |> Ash.Changeset.for_create(:create, %{name: "cool"})
        |> Domain.create!()

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:categories, [category1, category2],
          type: :append_and_remove
        )
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      [post] =
        Post
        |> Ash.Query.load(categories: :posts, author: [])
        |> Ash.Query.filter(id == ^post.id)
        |> Domain.read!(authorize?: true)

      post_id = post.id

      assert [%{posts: [%{id: ^post_id}]}, %{posts: [%{id: ^post_id}]}] = post.categories
    end

    test "it loads sorted relationships in the proper order" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "zerg"})
        |> Domain.create!()

      _post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post1"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      :timer.sleep(2)

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "post2"})
        |> Ash.Changeset.manage_relationship(:author, author, type: :append_and_remove)
        |> Domain.create!()

      [author] =
        Author
        |> Ash.Query.load(:latest_post)
        |> Domain.read!()

      assert author.latest_post.id == post2.id
    end
  end
end
