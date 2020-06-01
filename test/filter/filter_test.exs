defmodule Ash.Test.Filter.FilterTest do
  use ExUnit.Case, async: true

  defmodule Profile do
    use Ash.Resource, name: "profiles", type: "profile"
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
      belongs_to :user, Ash.Test.Filter.FilterTest.User
    end
  end

  defmodule User do
    use Ash.Resource, name: "users", type: "user"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
      update :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :name, :string
      attribute :allow_second_author, :boolean
    end

    relationships do
      has_many :posts, Ash.Test.Filter.FilterTest.Post,
        reverse_relationship: :author1,
        destination_field: :author1_id

      has_many :second_posts, Ash.Test.Filter.FilterTest.Post,
        reverse_relationship: :author2,
        destination_field: :author1_id

      has_one :profile, Profile
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
      belongs_to :source_post, Ash.Test.Filter.FilterTest.Post, primary_key?: true
      belongs_to :destination_post, Ash.Test.Filter.FilterTest.Post, primary_key?: true
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
      attribute :points, :integer
    end

    relationships do
      belongs_to :author1, User,
        destination_field: :id,
        source_field: :author1_id

      belongs_to :author2, User,
        destination_field: :id,
        source_field: :author2_id

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id,
        reverse_relationship: :related_posts
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, User, Profile, PostLink]
  end

  describe "simple attribute filters" do
    setup do
      post1 = Api.create!(Post, attributes: %{title: "title1", contents: "contents1", points: 1})
      post2 = Api.create!(Post, attributes: %{title: "title2", contents: "contents2", points: 2})

      %{post1: post1, post2: post2}
    end

    test "single filter field", %{post1: post1} do
      assert [^post1] =
               Post
               |> Api.query()
               |> Ash.Query.filter(title: post1.title)
               |> Api.read!()
    end

    test "multiple filter field matches", %{post1: post1} do
      assert [^post1] =
               Post
               |> Api.query()
               |> Ash.Query.filter(title: post1.title, contents: post1.contents)
               |> Api.read!()
    end

    test "no field matches" do
      assert [] =
               Post
               |> Api.query()
               |> Ash.Query.filter(title: "no match")
               |> Api.read!()
    end

    test "no field matches single record, but each matches one record", %{
      post1: post1,
      post2: post2
    } do
      assert [] =
               Post
               |> Api.query()
               |> Ash.Query.filter(title: post1.title, contents: post2.contents)
               |> Api.read!()
    end
  end

  describe "relationship filters" do
    setup do
      post1 = Api.create!(Post, attributes: %{title: "title1", contents: "contents1", points: 1})
      post2 = Api.create!(Post, attributes: %{title: "title2", contents: "contents2", points: 2})

      post3 =
        Api.create!(Post,
          attributes: %{title: "title3", contents: "contents3", points: 3},
          relationships: %{related_posts: [post1, post2]}
        )

      post4 =
        Api.create!(Post,
          attributes: %{title: "title4", contents: "contents3", points: 4},
          relationships: %{related_posts: [post3]}
        )

      profile1 = Api.create!(Profile, attributes: %{bio: "dope"})

      user1 =
        Api.create!(User,
          attributes: %{name: "broseph"},
          relationships: %{posts: [post1, post2], profile: profile1}
        )

      user2 = Api.create!(User, attributes: %{name: "broseph"}, relationships: %{posts: [post2]})

      profile2 = Api.create!(Profile, attributes: %{bio: "dope2"}, relationships: %{user: user2})

      %{
        post1: Api.reload!(post1),
        post2: Api.reload!(post2),
        post3: Api.reload!(post3),
        post4: Api.reload!(post4),
        profile1: Api.reload!(profile1),
        user1: Api.reload!(user1),
        user2: Api.reload!(user2),
        profile2: Api.reload!(profile2)
      }
    end

    test "filtering on a has_one relationship", %{profile2: profile2, user2: user2} do
      assert [^user2] =
               User
               |> Api.query()
               |> Ash.Query.filter(profile: profile2.id)
               |> Api.read!()
    end

    test "filtering on a belongs_to relationship", %{profile1: profile1, user1: user1} do
      assert [^profile1] =
               Profile
               |> Api.query()
               |> Ash.Query.filter(user: user1.id)
               |> Api.read!()
    end

    test "filtering on a has_many relationship", %{user2: user2, post2: post2} do
      assert [^user2] =
               User
               |> Api.query()
               |> Ash.Query.filter(posts: post2.id)
               |> Api.read!()
    end

    test "filtering on a many_to_many relationship", %{post4: post4, post3: post3} do
      assert [^post4] =
               Post
               |> Api.query()
               |> Ash.Query.filter(related_posts: post3.id)
               |> Api.read!()
    end
  end
end
