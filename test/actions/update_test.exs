defmodule Ash.Test.Actions.UpdateTest do
  use ExUnit.Case, async: true

  defmodule Profile do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

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
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

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
      has_one :profile, Profile

      has_many :posts, Ash.Test.Actions.UpdateTest.Post, reverse_relationship: :author
    end
  end

  defmodule PostLink do
    use Ash.Resource, name: "post_links", type: "post_link"
    use Ash.DataLayer.Ets, private?: true

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
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

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
        destination_field_on_join_table: :destination_post_id,
        reverse_relationship: :related_posts
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Author, Post, Profile, PostLink]
  end

  describe "simple updates" do
    test "allows updating a record with valid attributes" do
      post = Api.create!(Post, attributes: %{title: "foo", contents: "bar"})

      assert %Post{title: "bar", contents: "foo"} =
               Api.update!(post, attributes: %{title: "bar", contents: "foo"})
    end
  end

  describe "updating many to many relationships" do
    test "allows updating with a many_to_many relationship" do
      post = Api.create!(Post, attributes: %{title: "title"})
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      Api.update!(post, relationships: %{related_posts: [post2.id, post3.id]})
    end

    test "it updates the join table properly" do
      post = Api.create!(Post, attributes: %{title: "title"})
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      Api.update!(post, relationships: %{related_posts: [post2.id, post3.id]})

      assert [_, _] = Api.read!(PostLink)
    end

    test "it responds with the relationship filled in" do
      post = Api.create!(Post, attributes: %{title: "title"})
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      assert Enum.sort(
               Api.update!(post, relationships: %{related_posts: [post2.id, post3.id]}).related_posts
             ) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
    end
  end

  describe "updating with has_one relationships" do
    test "allows updating with has_one relationship" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})
      profile2 = Api.create!(Profile, attributes: %{bio: "second best dude"})

      author =
        Api.create!(Author,
          attributes: %{name: "fred"},
          relationships: %{profile: profile.id}
        )

      Api.update!(author, relationships: %{profile: profile2.id})
    end

    test "it sets the relationship on the destination record accordingly" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})
      profile2 = Api.create!(Profile, attributes: %{bio: "second best dude"})

      author =
        Api.create!(Author,
          attributes: %{name: "fred"},
          relationships: %{profile: profile.id}
        )

      Api.update!(author, relationships: %{profile: profile2.id})

      assert Api.get!(Profile, profile.id).author_id == nil
      assert Api.get!(Profile, profile2.id).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})
      profile2 = Api.create!(Profile, attributes: %{bio: "second best dude"})

      author =
        Api.create!(Author,
          attributes: %{name: "fred"},
          relationships: %{profile: profile.id}
        )

      updated_author = Api.update!(author, relationships: %{profile: profile2.id})
      assert updated_author.profile == %{profile2 | author_id: author.id}
    end
  end

  describe "updating with a has_many relationship" do
    test "allows updating with a has_many relationship" do
      post = Api.create!(Post, attributes: %{title: "sup"})
      post2 = Api.create!(Post, attributes: %{title: "sup2"})

      author =
        Api.create!(Author,
          attributes: %{title: "foobar"},
          relationships: %{
            posts: [post.id]
          }
        )

      Api.update!(author,
        relationships: %{
          posts: [post.id, post2.id]
        }
      )
    end

    test "it sets the relationship on the destination records accordingly" do
      post = Api.create!(Post, attributes: %{title: "sup"})
      post2 = Api.create!(Post, attributes: %{title: "sup2"})

      author =
        Api.create!(Author,
          attributes: %{title: "foobar"},
          relationships: %{
            posts: [post.id]
          }
        )

      author =
        Api.update!(author,
          relationships: %{
            posts: [post2.id]
          }
        )

      assert Api.get!(Post, post.id).author_id == nil
      assert Api.get!(Post, post2.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      post = Api.create!(Post, attributes: %{title: "sup"})
      post2 = Api.create!(Post, attributes: %{title: "sup2"})

      author =
        Api.create!(Author,
          attributes: %{title: "foobar"},
          relationships: %{
            posts: [post.id]
          }
        )

      assert Api.update!(author,
               relationships: %{
                 posts: [post2.id]
               }
             ).posts == [Api.get!(Post, post2.id)]
    end
  end

  describe "updating with belongs_to relationships" do
    test "allows updating with belongs_to relationship" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})
      author2 = Api.create!(Author, attributes: %{bio: "best dude"})

      post =
        Api.create!(Post,
          attributes: %{title: "foobar"},
          relationships: %{
            author: author.id
          }
        )

      Api.update!(post, relationships: %{author: author2.id})
    end

    test "sets the relationship on the destination records accordingly" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})
      author2 = Api.create!(Author, attributes: %{bio: "best dude"})

      post =
        Api.create!(Post,
          attributes: %{title: "foobar"},
          relationships: %{
            author: author.id
          }
        )

      Api.update!(post, relationships: %{author: author2.id})

      assert Api.get!(Author, author2.id, side_load: [:posts]).posts == [Api.get!(Post, post.id)]
    end

    test "it respons with the relationship field filled in" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})
      author2 = Api.create!(Author, attributes: %{bio: "best dude"})

      post =
        Api.create!(Post,
          attributes: %{title: "foobar"},
          relationships: %{
            author: author.id
          }
        )

      assert Api.update!(post, relationships: %{author: author2.id}).author ==
               Api.get!(Author, author2.id)
    end
  end
end
