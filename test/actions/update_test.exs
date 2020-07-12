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
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
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
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :bio, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.UpdateTest.Author
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
      update :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
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
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      belongs_to :author, Author

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id
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
        |> create(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert %Post{title: "bar", contents: "foo"} =
               post |> update(%{title: "bar", contents: "foo"}) |> Api.update!()
    end
  end

  describe "updating many to many relationships" do
    test "allows updating with a many_to_many relationship" do
      post =
        Post
        |> create(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> create(%{title: "title3"})
        |> Api.create!()

      post
      |> update()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.update!()
    end

    test "it updates the join table properly" do
      post =
        Post
        |> create(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> create(%{title: "title3"})
        |> Api.create!()

      post
      |> update()
      |> replace_relationship(:related_posts, [post2, post3])
      |> Api.update!()

      assert [_, _] = Api.read!(PostLink)
    end

    test "it responds with the relationship filled in" do
      post =
        Post
        |> create(%{title: "title"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "title2"})
        |> Api.create!()

      post3 =
        Post
        |> create(%{title: "title3"})
        |> Api.create!()

      new_post =
        post |> update() |> replace_relationship(:related_posts, [post2, post3]) |> Api.update!()

      assert Enum.sort(new_post.related_posts) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
    end
  end

  describe "updating with has_one relationships" do
    test "allows updating with has_one relationship" do
      profile =
        Profile
        |> create(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> create(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      author
      |> update()
      |> replace_relationship(:profile, profile2)
      |> Api.update!()
    end

    test "it sets the relationship on the destination record accordingly" do
      profile =
        Profile
        |> create(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> create(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      author
      |> update()
      |> replace_relationship(:profile, profile2)
      |> Api.update!()

      assert Api.get!(Profile, profile.id).author_id == nil
      assert Api.get!(Profile, profile2.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile =
        Profile
        |> create(%{bio: "best dude"})
        |> Api.create!()

      profile2 =
        Profile
        |> create(%{bio: "second best dude"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "fred"})
        |> replace_relationship(:profile, profile)
        |> Api.create!()

      updated_author =
        author
        |> update()
        |> replace_relationship(:profile, profile2)
        |> Api.update!()

      assert updated_author.profile == %{profile2 | author_id: author.id}
    end
  end

  describe "updating with a has_many relationship" do
    test "allows updating with a has_many relationship" do
      post =
        Post
        |> create(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      author
      |> update()
      |> replace_relationship(:posts, [post, post2])
      |> Api.update!()
    end

    test "it sets the relationship on the destination records accordingly" do
      post =
        Post
        |> create(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      author =
        author
        |> update()
        |> replace_relationship(:posts, [post2.id])
        |> Api.update!()

      assert Api.get!(Post, post.id).author_id == nil
      assert Api.get!(Post, post2.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      post =
        Post
        |> create(%{title: "sup"})
        |> Api.create!()

      post2 =
        Post
        |> create(%{title: "sup2"})
        |> Api.create!()

      author =
        Author
        |> create(%{name: "foobar"})
        |> replace_relationship(:posts, [post])
        |> Api.create!()

      updated_author =
        author
        |> update()
        |> replace_relationship(:posts, [post2])
        |> Api.update!()

      assert updated_author.posts == [Api.get!(Post, post2.id)]
    end
  end

  describe "updating with belongs_to relationships" do
    test "allows updating with belongs_to relationship" do
      author =
        Author
        |> create(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> create(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> create(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      post
      |> update()
      |> replace_relationship(:author, author2)
      |> Api.update!()
    end

    test "sets the relationship on the destination records accordingly" do
      author =
        Author
        |> create(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> create(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> create(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      post
      |> update()
      |> replace_relationship(:author, author2)
      |> Api.update!()

      assert Api.get!(Author, author2.id, side_load: [:posts]).posts == [Api.get!(Post, post.id)]
    end

    test "it responds with the relationship field filled in" do
      author =
        Author
        |> create(%{name: "best dude"})
        |> Api.create!()

      author2 =
        Author
        |> create(%{name: "best dude2"})
        |> Api.create!()

      post =
        Post
        |> create(%{title: "foobar"})
        |> replace_relationship(:author, author)
        |> Api.create!()

      updated_post = post |> update() |> replace_relationship(:author, author2) |> Api.update!()

      assert updated_post.author ==
               Api.get!(Author, author2.id)
    end
  end

  describe "unauthorized update" do
    test "it does not update the record" do
      record =
        Authorized
        |> create(%{name: "bar"})
        |> Api.create!()

      assert_raise(Ash.Error.Forbidden, fn ->
        record
        |> update(%{name: "foo"})
        |> Api.update!(authorize?: true)
      end)

      assert Api.get!(Authorized, record.id).name == "bar"
    end
  end
end
