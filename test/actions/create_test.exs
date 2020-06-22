defmodule Ash.Test.Actions.CreateTest do
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
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

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
      attribute :date, :date
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.CreateTest.Author
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

      has_many :posts, Ash.Test.Actions.CreateTest.Post,
        reverse_relationship: :author,
        destination_field: :author
    end
  end

  defmodule PostDefaults do
    @moduledoc false
    def garbage2, do: "garbage2"
    def garbage3, do: "garbage3"
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read :default

      create :default
      update :default
    end

    relationships do
      belongs_to :source_post, Ash.Test.Actions.CreateTest.Post, primary_key?: true
      belongs_to :destination_post, Ash.Test.Actions.CreateTest.Post, primary_key?: true
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
      attribute :tag, :string, default: {:constant, "garbage"}
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3, []}
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

  describe "simple creates" do
    test "allows creating a record with valid attributes" do
      assert %Post{title: "foo", contents: "bar"} =
               Api.create!(Post,
                 attributes: %{title: "foo", contents: "bar", date: Date.utc_today()}
               )
    end

    test "constant default values are set properly" do
      assert %Post{tag: "garbage"} = Api.create!(Post, attributes: %{title: "foo"})
    end

    test "constant functions values are set properly" do
      assert %Post{tag2: "garbage2"} = Api.create!(Post, attributes: %{title: "foo"})
    end

    test "constant module/function values are set properly" do
      assert %Post{tag3: "garbage3"} = Api.create!(Post, attributes: %{title: "foo"})
    end
  end

  describe "creating many to many relationships" do
    test "allows creating with a many_to_many relationship" do
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      Api.create!(Post, relationships: %{related_posts: [post2.id, post3.id]})
    end

    test "it updates the join table properly" do
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      Api.create!(Post, relationships: %{related_posts: [post2.id, post3.id]})

      assert [_, _] =
               PostLink
               |> Api.query()
               |> Api.read!()
    end

    test "it responds with the relationship filled in" do
      post2 = Api.create!(Post, attributes: %{title: "title2"})
      post3 = Api.create!(Post, attributes: %{title: "title3"})

      assert Enum.sort(
               Api.create!(Post, relationships: %{related_posts: [post2.id, post3.id]}).related_posts
             ) ==
               Enum.sort([
                 Api.get!(Post, post2.id),
                 Api.get!(Post, post3.id)
               ])
    end
  end

  describe "creating with has_one relationships" do
    test "allows creating with has_one relationship" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})

      Api.create!(Author,
        attributes: %{name: "fred"},
        relationships: %{profile: profile.id}
      )
    end

    test "it sets the relationship on the destination record accordingly" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})

      author =
        Api.create!(Author,
          attributes: %{name: "fred"},
          relationships: %{profile: profile.id}
        )

      assert Api.get!(Profile, profile.id).author_id == author.id
    end

    test "it responds with the relationshi filled in" do
      profile = Api.create!(Profile, attributes: %{bio: "best dude"})

      author =
        Api.create!(Author,
          attributes: %{name: "fred"},
          relationships: %{profile: profile.id}
        )

      assert author.profile.author_id == author.id
    end
  end

  describe "creating with a has_many relationship" do
    test "allows creating with a has_many relationship" do
      post = Api.create!(Post, attributes: %{title: "sup"})

      Api.create!(Author,
        attributes: %{title: "foobar"},
        relationships: %{
          posts: [post.id]
        }
      )
    end
  end

  describe "creating with belongs_to relationships" do
    test "allows creating with belongs_to relationship" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})

      Api.create!(Post,
        attributes: %{title: "foobar"},
        relationships: %{
          author: author.id
        }
      )
    end

    test "it sets the relationship on the destination record accordingly" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})

      post =
        Api.create!(Post,
          attributes: %{title: "foobar"},
          relationships: %{
            author: author.id
          }
        )

      assert Api.get!(Post, post.id).author_id == author.id
    end

    test "it responds with the relationship field filled in" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})

      assert Api.create!(Post,
               attributes: %{title: "foobar"},
               relationships: %{
                 author: author.id
               }
             ).author_id == author.id
    end

    test "it responds with the relationship filled in" do
      author = Api.create!(Author, attributes: %{bio: "best dude"})

      assert Api.create!(Post,
               attributes: %{title: "foobar"},
               relationships: %{
                 author: author.id
               }
             ).author == author
    end
  end

  describe "unauthorized create" do
    test "it does not create the record" do
      assert_raise(Ash.Error.Forbidden, fn ->
        Api.create!(Authorized, attributes: %{name: "foo"}, authorize?: true)
      end)

      assert [] = Api.read!(Authorized)
    end
  end
end
