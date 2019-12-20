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
      attribute :bio, :string
    end

    relationships do
      belongs_to :user, Ash.Test.Filter.FilterTest.User,
        source_field: :user_id,
        destination_field: :id
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
      attribute :name, :string
      attribute :allow_second_author, :boolean
    end

    relationships do
      has_many :posts, Ash.Test.Filter.FilterTest.Post

      has_one :profile, Profile,
        destination_field: :user_id,
        source_field: :id
    end
  end

  defmodule PostLink do
    use Ash.Resource, name: "post_links", type: "post_link", primary_key: false
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
        destination_field_on_join_table: :destination_post_id
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, User, Profile]
  end

  describe "simple attribute filters" do
    setup do
      post1 = Api.create!(Post, attributes: %{title: "title1", contents: "contents1", points: 1})
      post2 = Api.create!(Post, attributes: %{title: "title2", contents: "contents2", points: 2})

      %{post1: post1, post2: post2}
    end

    test "single filter field", %{post1: post1} do
      assert %{results: [^post1]} =
               Api.read!(Post,
                 filter: [
                   title: post1.title
                 ]
               )
    end

    test "multiple filter field matches", %{post1: post1} do
      assert %{results: [^post1]} =
               Api.read!(Post,
                 filter: [
                   title: post1.title,
                   contents: post1.contents
                 ]
               )
    end

    test "no field matches" do
      assert %{results: []} =
               Api.read!(Post,
                 filter: [
                   title: "no match"
                 ]
               )
    end

    test "no field matches single record, but each matches one record", %{
      post1: post1,
      post2: post2
    } do
      assert %{results: []} =
               Api.read!(Post,
                 filter: [
                   title: post1.title,
                   contents: post2.contents
                 ]
               )
    end
  end

  describe "relationship filters" do
    setup do
      post1 = Api.create!(Post, attributes: %{title: "title1", contents: "contents1", points: 1})
      post2 = Api.create!(Post, attributes: %{title: "title2", contents: "contents2", points: 2})

      # post3 =
      #   Api.create!(Post,
      #     attributes: %{title: "title3", contents: "contents3", points: 3},
      #     relationships: %{related_posts: [post1, post2]}
      #   )

      profile1 = Api.create!(Profile, attributes: %{bio: "dope"})

      user1 =
        Api.create!(User,
          attributes: %{name: "broseph"},
          relationships: %{posts: [post1, post2], profile: profile1}
        )

      user2 = Api.create!(User, attributes: %{name: "broseph"}, relationships: %{posts: [post2]})

      profile2 = Api.create!(Profile, attributes: %{bio: "dope2"}, relationships: %{user: user2})

      %{post1: post1, post2: post2}
    end

    test "it works" do
      assert true
    end
  end
end
