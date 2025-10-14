# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id

      attribute :bio, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.DestroyTest.Author do
        public?(true)
      end
    end
  end

  defmodule ManualDestroyAuthor do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _changeset, data ->
        Ash.destroy!(data)

        {:ok, data}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      destroy :soft_with_confirm do
        accept []
        soft? true
        require_atomic? false
        argument :confirm, :string, allow_nil?: false

        change fn changeset, _ ->
          Ash.Changeset.before_action(changeset, fn changeset ->
            if changeset.arguments[:confirm] == "CONFIRM" do
              changeset
            else
              Ash.Changeset.add_error(changeset,
                field: :confirm,
                message: "Type CONFIRM to confirm"
              )
            end
          end)
        end
      end

      destroy :manual do
        accept []

        manual fn changeset, _ ->
          Ash.destroy(changeset.data, return_destroyed?: true)
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      has_one :profile, Profile, destination_attribute: :author_id, public?: true

      has_many :posts, Ash.Test.Actions.DestroyTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule PostDefaults do
    @moduledoc false
    def garbage2, do: "garbage2"
    def garbage3, do: "garbage3"
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :type, :string do
        public?(true)
      end
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Actions.UpdateTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule AtomicOnlyValidation do
    use Ash.Resource.Validation

    @impl true
    def atomic(_, _, _) do
      :ok
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    validations do
      validate AtomicOnlyValidation,
        on: [:destroy]
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :contents, :string, public?: true
      attribute :tag, :string, default: "garbage", public?: true
      attribute :tag2, :string, default: &PostDefaults.garbage2/0, public?: true
      attribute :tag3, :string, default: {PostDefaults, :garbage3, []}, public?: true
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
    end
  end

  describe "simple destroy" do
    test "allows destroying a record" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      assert Ash.destroy!(post) == :ok

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "before action hooks are run" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      assert_raise Ash.Error.Invalid, ~r/Type CONFIRM to confirm/, fn ->
        author
        |> Ash.Changeset.for_destroy(:soft_with_confirm, %{confirm: "NOT CONFIRMED"})
        |> Ash.destroy!()
      end
    end

    test "returns the record if requested" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}} = Ash.destroy(post, return_destroyed?: true)

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "returns the record and notifications if requested" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}, [_]} =
               Ash.destroy(post, return_destroyed?: true, return_notifications?: true)

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "the destroy does not happen if it is unauthorized" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foobar"})
        |> Ash.create!(authorize?: false)

      start_supervised({Ash.Test.Authorizer, strict_check: :continue, check: :forbidden})

      assert_raise(Ash.Error.Forbidden, fn ->
        Ash.destroy!(author, authorize?: true)
      end)

      assert Ash.get!(Author, author.id, authorize?: false)
    end
  end

  describe "load" do
    test "allows loading has_many relationship on the changeset" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(posts: load_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading has_many relationship on the action options" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [posts: load_query])

      assert [%Post{title: "Post 1"}, %Post{title: "Post 2"}] = author.posts
    end

    test "allows loading paginated has_many relationship on the changeset" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(posts: offset_pagination_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = author.posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(posts: keyset_pagination_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = author.posts
    end

    test "allows loading paginated has_many relationship on the action options" do
      post1 = Ash.create!(Post, %{title: "Post 1"})
      post2 = Ash.create!(Post, %{title: "Post 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [posts: offset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = author.posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "Author"})
        |> Ash.Changeset.manage_relationship(:posts, [post2, post1], type: :append_and_remove)
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [posts: keyset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Post 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = author.posts
    end

    test "allows loading many_to_many relationship on the changeset" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(related_posts: load_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading many_to_many relationship on the action options" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      load_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [related_posts: load_query])

      assert [%Post{title: "Related 1"}, %Post{title: "Related 2"}] = post.related_posts
    end

    test "allows loading paginated many_to_many relationship on the changeset" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(related_posts: offset_pagination_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = post.related_posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.Changeset.for_destroy(:destroy, %{})
        |> Ash.Changeset.load(related_posts: keyset_pagination_query)
        |> Ash.destroy!(return_destroyed?: true)

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = post.related_posts
    end

    test "allows loading paginated many_to_many relationship on the action options" do
      related_post1 = Ash.create!(Post, %{title: "Related 1"})
      related_post2 = Ash.create!(Post, %{title: "Related 2"})

      offset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [related_posts: offset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 1", __metadata__: %{keyset: keyset}}],
               limit: 1,
               count: 2,
               more?: true
             } = post.related_posts

      keyset_pagination_query =
        Post
        |> Ash.Query.sort(title: :asc)
        |> Ash.Query.select([:title])
        |> Ash.Query.page(count: true, limit: 1, after: keyset)

      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "Title"})
        |> Ash.Changeset.manage_relationship(:related_posts, [related_post2, related_post1],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> Ash.destroy!(return_destroyed?: true, load: [related_posts: keyset_pagination_query])

      assert %Ash.Page.Keyset{
               results: [%Post{title: "Related 2"}],
               limit: 1,
               count: 2,
               more?: false,
               before: nil,
               after: ^keyset
             } = post.related_posts
    end
  end

  describe "manual destroy" do
    test "allows destroying a record" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      assert Ash.destroy!(author, action: :manual) == :ok

      refute Ash.read_one!(Ash.Query.filter(Author, id == ^author.id))
    end
  end
end
