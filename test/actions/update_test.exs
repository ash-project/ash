defmodule Ash.Test.Actions.UpdateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Authorized do
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    actions do
      read :default
      create :default
      update :default
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
      update :default
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.UpdateTest.Author
    end
  end

  defmodule DuplicateName do
    use Ash.Resource.Change

    def change(changeset, _, _) do
      case Ash.Changeset.fetch_change(changeset, :name) do
        :error -> changeset
        {:ok, name} -> Ash.Changeset.change_attribute(changeset, :name, name <> name)
      end
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
      update :default, primary?: true

      update :only_allow_name do
        accept([:name])
      end

      update :duplicate_name do
        change {DuplicateName, []}
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
      attribute :bio, :string
    end

    relationships do
      has_one :profile, Profile, destination_field: :author_id

      has_many :posts, Ash.Test.Actions.UpdateTest.Post, destination_field: :author_id
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    attributes do
      attribute :type, :string
    end

    actions do
      read :default

      create :default
      update :default
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.UpdateTest.Post, primary_key?: true
      belongs_to :destination_post, Ash.Test.Actions.UpdateTest.Post, primary_key?: true
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
      update :default
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id,
        join_attributes: [:type]
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Author)
      resource(Post)
      resource(Profile)
      resource(PostLink)
      resource(Authorized)
    end
  end

  import Ash.Changeset

  describe "simple updates" do
    test "allows updating a record with valid attributes" do
      post =
        Post
        |> new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert %Post{title: "bar", contents: "foo"} =
               post |> new(%{title: "bar", contents: "foo"}) |> Api.update!()
    end
  end

  describe "allow" do
    test "allows attributes in the list" do
      author =
        Author
        |> new(%{name: "fred"})
        |> Api.create!()

      author
      |> new(%{name: "joe"})
      |> Api.update!(action: :only_allow_name)
    end

    test "does not allow attributes in the list" do
      author =
        Author
        |> new(%{name: "fred"})
        |> Api.create!()

      assert_raise Ash.Error.Invalid, ~r/Invalid value provided for bio: cannot be changed/, fn ->
        author
        |> new(%{bio: "bio"})
        |> Api.update!(action: :only_allow_name)
      end
    end
  end

  describe "changeset" do
    test "changes are run properly" do
      author =
        Author
        |> new(%{name: "fred"})
        |> Api.create!()

      author =
        author
        |> new(%{name: "joe"})
        |> Api.update!(action: :duplicate_name)

      assert author.name == "joejoe"
    end
  end

  describe "updating many to many relationships" do
    test "allows updating with a many_to_many relationship" do
      post =
        Post
        |> new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "title3"})
        |> Api.create!()

      post
      |> new()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.update!()
    end

    test "it updates the join table properly" do
      post =
        Post
        |> new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "title3"})
        |> Api.create!()

      post
      |> new()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.update!()

      assert [_, _] = Api.read!(PostLink)
    end

    test "it responds with the relationship filled in" do
      post =
        Post
        |> new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "title3"})
        |> Api.create!()

      new_post =
        post |> new() |> replace_relationship(:related_posts, [post2, post3]) |> Api.update!()

      assert Enum.sort(new_post.related_posts) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
    end

    test "it updates any join fields" do
      post =
        Post
        |> new(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "title3"})
        |> Api.create!()

      new_post =
        post
        |> new()
        |> replace_relationship(:related_posts, [{post2, %{type: "a"}}, {post3, %{type: "b"}}])
        |> Api.update!()

      types = Enum.sort(Enum.map(new_post.related_posts_join_assoc, &Map.get(&1, :type)))

      assert types == ["a", "b"]

      new_post =
        new_post
        |> new()
        |> replace_relationship(:related_posts, [{post2, %{type: "c"}}, {post3, %{type: "d"}}])
        |> Api.update!()

      types = Enum.sort(Enum.map(new_post.related_posts_join_assoc, &Map.get(&1, :type)))

      assert types == ["c", "d"]
    end
  end

  describe "updating with has_one relationships" do
    test "allows updating with has_one relationship" do
      profile =
        Profile
        |> new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      author
      |> new()
      |> replace_relationship(:profile, profile2)
      |> Api.update!()
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      author
      |> new()
      |> replace_relationship(:profile, profile2)
      |> Api.update!()

      assert Api.get!(Profile, profile.id).author_id == nil
      assert Api.get!(Profile, profile2.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> new(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> new(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      updated_author =
        author
        |> new()
        |> replace_relationship(:profile, profile2)
        |> Api.update!()

      assert updated_author.profile == %{profile2 | author_id: author.id}
    end
  end

  describe "updating with a has_many relationship" do
    test "allows updating with a has_many relationship" do
      post =
        Post
        |> new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      author
      |> new()
      |> replace_relationship(:posts, [post, post2])
      |> Api.update!()
    end

    test "it sets the relationship on the destination records accordingly" do
      post =
        Post
        |> new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      author =
        author
        |> new()
        |> replace_relationship(:posts, [post2.id])
        |> Api.update!()

      assert Api.get!(Post, post.id).author_id == nil
      assert Api.get!(Post, post2.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      post =
        Post
        |> new(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> new(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      updated_author =
        author
        |> new()
        |> replace_relationship(:posts, [post2])
        |> Api.update!()

      assert updated_author.posts == [Api.get!(Post, post2.id)]
    end
  end

  describe "updating with belongs_to relationships" do
    test "allows updating with belongs_to relationship" do
      author =
        Author
        |> new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      post
      |> new()
      |> replace_relationship(:author, author2)
      |> Api.update!()
    end

    test "sets the relationship on the destination records accordingly" do
      author =
        Author
        |> new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      post
      |> new()
      |> replace_relationship(:author, author2)
      |> Api.update!()

      assert Api.get!(Author, author2.id, load: [:posts]).posts == [Api.get!(Post, post.id)]
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> new(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> new(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> new(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      updated_post = post |> new() |> replace_relationship(:author, author2) |> Api.update!()

      assert updated_post.author ==
               Api.get!(Author, author2.id)
    end
  end

  describe "unauthorized update" do
    test "it does not update the record" do
      record =
        Authorized
        |> new(%{name: "bar"})
        |> Api.create!()

      assert_raise(Ash.Error.Forbidden, fn ->
        record
        |> new(%{name: "foo"})
        |> Api.update!(authorize?: true)
      end)

      assert Api.get!(Authorized, record.id).name == "bar"
    end
  end
end
