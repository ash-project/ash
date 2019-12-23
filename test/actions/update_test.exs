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
      attribute :name, :string
    end

    relationships do
      has_one :profile, Profile

      has_many :posts, Ash.Test.Actions.UpdateTest.Post
    end
  end

  defmodule PostDefaults do
    def garbage2(), do: "garbage2"
    def garbage3(), do: "garbage3"
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
      attribute :title, :string
      attribute :contents, :string
      attribute :tag, :string, default: {:constant, "garbage"}
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3}
    end

    relationships do
      belongs_to :author, Author
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Author, Post, Profile]
  end

  describe "simple updates" do
    test "allows updating a record with valid attributes" do
      post = Api.create!(Post, attributes: %{title: "foo", contents: "bar"})

      assert %Post{title: "bar", contents: "foo"} =
               Api.update!(post, attributes: %{title: "bar", contents: "foo"})
    end
  end

  describe "updating with has_one relationships" do
    test "allows creating with has_one relationship" do
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
  end

  # describe "creating with belongs_to relationships" do
  #   test "allows creating with belongs_to relationship" do
  #     author = Api.create!(Author, attributes: %{bio: "best dude"})

  #     Api.create!(Post,
  #       attributes: %{title: "foobar"},
  #       relationships: %{
  #         author: author.id
  #       }
  #     )
  #   end

  #   test "it sets the relationship on the destination record accordingly" do
  #     author = Api.create!(Author, attributes: %{bio: "best dude"})

  #     post =
  #       Api.create!(Post,
  #         attributes: %{title: "foobar"},
  #         relationships: %{
  #           author: author.id
  #         }
  #       )

  #     assert Api.get!(Post, post.id).author_id == author.id
  #   end

  #   test "it responds with the relationship field filled in" do
  #     author = Api.create!(Author, attributes: %{bio: "best dude"})

  #     assert Api.create!(Post,
  #              attributes: %{title: "foobar"},
  #              relationships: %{
  #                author: author.id
  #              }
  #            ).author_id == author.id
  #   end

  #   test "it responds with the relationship filled in" do
  #     author = Api.create!(Author, attributes: %{bio: "best dude"})

  #     assert Api.create!(Post,
  #              attributes: %{title: "foobar"},
  #              relationships: %{
  #                author: author.id
  #              }
  #            ).author == author
  #   end
  # end
end
