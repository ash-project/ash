defmodule Ash.Test.Filter.FilterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Filter

  require Ash.Query

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
      attribute :private, :string, private?: true
    end

    relationships do
      belongs_to :user, Ash.Test.Filter.FilterTest.User
    end
  end

  defmodule User do
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
      attribute :name, :string
      attribute :allow_second_author, :boolean
    end

    relationships do
      has_many :posts, Ash.Test.Filter.FilterTest.Post, destination_field: :author1_id

      has_many :second_posts, Ash.Test.Filter.FilterTest.Post, destination_field: :author1_id

      has_one :profile, Profile, destination_field: :user_id
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
      belongs_to :source_post, Ash.Test.Filter.FilterTest.Post,
        primary_key?: true,
        required?: true

      belongs_to :destination_post, Ash.Test.Filter.FilterTest.Post,
        primary_key?: true,
        required?: true
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
      attribute :points, :integer
      attribute :approved_at, :utc_datetime
      attribute :category, :ci_string
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

  defmodule SoftDeletePost do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    resource do
      base_filter is_nil: :deleted_at
    end

    actions do
      read :default
      create :default

      destroy :default do
        soft? true

        change set_attribute(:deleted_at, &DateTime.utc_now/0)
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :deleted_at, :utc_datetime
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Post)
      resource(SoftDeletePost)
      resource(User)
      resource(Profile)
      resource(PostLink)
    end
  end

  import Ash.Changeset

  describe "simple attribute filters" do
    setup do
      post1 =
        Post
        |> new(%{title: "title1", contents: "contents1", points: 1})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2", contents: "contents2", points: 2})
        |> Api.create!()

      %{post1: post1, post2: post2}
    end

    test "single filter field", %{post1: post1} do
      assert [^post1] =
               Post
               |> Ash.Query.filter(title == ^post1.title)
               |> Api.read!()
    end

    test "multiple filter field matches", %{post1: post1} do
      assert [^post1] =
               Post
               |> Ash.Query.filter(title == ^post1.title and contents == ^post1.contents)
               |> Api.read!()
    end

    test "no field matches" do
      assert [] =
               Post
               |> Ash.Query.filter(title == "no match")
               |> Api.read!()
    end

    test "no field matches single record, but each matches one record", %{
      post1: post1,
      post2: post2
    } do
      assert [] =
               Post
               |> Ash.Query.filter(title == ^post1.title and contents == ^post2.contents)
               |> Api.read!()
    end

    test "less than works", %{
      post1: post1,
      post2: post2
    } do
      assert [^post1] =
               Post
               |> Ash.Query.filter(points < 2)
               |> Api.read!()

      assert [^post1, ^post2] =
               Post
               |> Ash.Query.filter(points < 3)
               |> Ash.Query.sort(points: :asc)
               |> Api.read!()
    end

    test "greater than works", %{
      post1: post1,
      post2: post2
    } do
      assert [^post2] =
               Post
               |> Ash.Query.filter(points > 1)
               |> Api.read!()

      assert [^post1, ^post2] =
               Post
               |> Ash.Query.filter(points > 0)
               |> Ash.Query.sort(points: :asc)
               |> Api.read!()
    end
  end

  describe "relationship filters" do
    setup do
      post1 =
        Post
        |> new(%{title: "title1", contents: "contents1", points: 1})
        |> Api.create!()

      post2 =
        Post
        |> new(%{title: "title2", contents: "contents2", points: 2})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "title3", contents: "contents3", points: 3})
        |> replace_relationship(:related_posts, [post1, post2])
        |> Api.create!()

      post4 =
        Post
        |> new(%{title: "title4", contents: "contents4", points: 4})
        |> replace_relationship(:related_posts, [post3])
        |> Api.create!()

      profile1 =
        Profile
        |> new(%{bio: "dope"})
        |> Api.create!()

      user1 =
        User
        |> new(%{name: "broseph"})
        |> replace_relationship(:posts, [post1, post2])
        |> replace_relationship(:profile, profile1)
        |> Api.create!()

      user2 =
        User
        |> new(%{name: "broseph"})
        |> replace_relationship(:posts, [post2])
        |> Api.create!()

      profile2 =
        Profile
        |> new(%{bio: "dope2"})
        |> replace_relationship(:user, user2)
        |> Api.create!()

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

    test "filtering on a has_one relationship", %{profile2: profile2, user2: %{id: user2_id}} do
      assert [%{id: ^user2_id}] =
               User
               |> Ash.Query.filter(profile == ^profile2.id)
               |> Api.read!()
    end

    test "filtering on a belongs_to relationship", %{profile1: %{id: id}, user1: user1} do
      assert [%{id: ^id}] =
               Profile
               |> Ash.Query.filter(user == ^user1.id)
               |> Api.read!()
    end

    test "filtering on a has_many relationship", %{user2: %{id: user2_id}, post2: post2} do
      assert [%{id: ^user2_id}] =
               User
               |> Ash.Query.filter(posts == ^post2.id)
               |> Api.read!()
    end

    test "filtering on a many_to_many relationship", %{post4: %{id: post4_id}, post3: post3} do
      assert [%{id: ^post4_id}] =
               Post
               |> Ash.Query.filter(related_posts == ^post3.id)
               |> Api.read!()
    end
  end

  describe "filter subset logic" do
    test "can detect a filter is a subset of itself" do
      filter = Filter.parse!(Post, %{points: 1})

      assert Filter.strict_subset_of?(filter, filter)
    end

    test "can detect a filter is a subset of itself *and* something else" do
      filter = Filter.parse!(Post, points: 1)

      candidate = Filter.add_to_filter!(filter, title: "Title")

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect a filter is not a subset of itself *or* something else" do
      filter = Filter.parse!(Post, points: 1)

      candidate = Filter.add_to_filter!(filter, [title: "Title"], :or)

      refute Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect a filter is a subset based on a simplification" do
      filter = Filter.parse!(Post, points: [in: [1, 2]])

      candidate = Filter.parse!(Post, points: 1)

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect a filter is not a subset based on a simplification" do
      filter = Filter.parse!(Post, points: [in: [1, 2]])

      candidate = Filter.parse!(Post, points: 3)

      refute Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect a more complicated scenario" do
      filter = Filter.parse!(Post, or: [[points: [in: [1, 2, 3]]], [points: 4], [points: 5]])

      candidate = Filter.parse!(Post, or: [[points: 1], [points: 3], [points: 5]])

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect less than and greater than closing in on a single value" do
      filter = Filter.parse!(Post, points: [greater_than: 1, less_than: 3])

      candidate = Filter.parse!(Post, points: 2)

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "doesnt have false positives on less than and greater than closing in on a single value" do
      filter = Filter.parse!(Post, points: [greater_than: 1, less_than: 3])

      candidate = Filter.parse!(Post, points: 4)

      refute Filter.strict_subset_of?(filter, candidate)
    end

    test "understands unrelated negations" do
      filter = Filter.parse!(Post, or: [[points: [in: [1, 2, 3]]], [points: 4], [points: 5]])

      candidate =
        Filter.parse!(Post, or: [[points: 1], [points: 3], [points: 5]], not: [points: 7])

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "understands relationship filter subsets" do
      id1 = Ash.UUID.generate()
      id2 = Ash.UUID.generate()
      filter = Filter.parse!(Post, author1: [id: [in: [id1, id2]]])

      candidate = Filter.parse!(Post, author1: id1)

      assert Filter.strict_subset_of?(filter, candidate)
    end

    test "understands relationship filter subsets when a value coincides with the join field" do
      id1 = Ash.UUID.generate()
      id2 = Ash.UUID.generate()
      filter = Filter.parse!(Post, author1: [id: [in: [id1, id2]]])

      candidate = Filter.parse!(Post, author1_id: id1)

      assert Filter.strict_subset_of?(filter, candidate)
    end
  end

  describe "parse_input" do
    test "parse_input works when no private attributes are used" do
      Ash.Filter.parse_input!(Profile, bio: "foo")
    end

    test "parse_input fails when a private attribute is used" do
      Ash.Filter.parse!(Profile, private: "private")

      assert_raise(Ash.Error.Query.NoSuchAttributeOrRelationship, fn ->
        Ash.Filter.parse_input!(Profile, private: "private")
      end)
    end
  end

  describe "base_filter" do
    test "resources that apply to the base filter are returned" do
      %{id: id} =
        SoftDeletePost
        |> new(%{})
        |> Api.create!()

      assert [%{id: ^id}] = Api.read!(SoftDeletePost)
    end

    test "resources that don't apply to the base filter are not returned" do
      SoftDeletePost
      |> new(%{})
      |> Api.create!()
      |> Api.destroy()

      assert [] = Api.read!(SoftDeletePost)
    end
  end

  describe "contains/2" do
    test "works for simple strings" do
      Post
      |> new(%{title: "foobar"})
      |> Api.create!()

      Post
      |> new(%{title: "bazbuz"})
      |> Api.create!()

      assert [%{title: "foobar"}] =
               Post
               |> Ash.Query.filter(contains(title, "oba"))
               |> Api.read!()
    end

    test "works for simple strings with a case insensitive search term" do
      Post
      |> new(%{title: "foobar"})
      |> Api.create!()

      Post
      |> new(%{title: "bazbuz"})
      |> Api.create!()

      assert [%{title: "foobar"}] =
               Post
               |> Ash.Query.filter(contains(title, ^%Ash.CiString{string: "OBA"}))
               |> Api.read!()
    end

    test "works for case insensitive strings" do
      Post
      |> new(%{category: "foobar"})
      |> Api.create!()

      Post
      |> new(%{category: "bazbuz"})
      |> Api.create!()

      assert [%{category: %Ash.CiString{string: "foobar"}}] =
               Post
               |> Ash.Query.filter(contains(category, "OBA"))
               |> Api.read!()
    end
  end

  describe "calls in filters" do
    test "calls are evaluated and can be used in predicates" do
      post1 =
        Post
        |> new(%{title: "title1", contents: "contents1", points: 2})
        |> Api.create!()

      post_id = post1.id

      assert [%Post{id: ^post_id}] =
               Post
               |> Ash.Query.filter(points + 1 == 3)
               |> Api.read!()
    end

    test "function calls are evaluated properly" do
      post1 =
        Post
        |> new(%{title: "title1", approved_at: Timex.shift(Timex.now(), weeks: -1)})
        |> Api.create!()

      Post
      |> new(%{title: "title1", approved_at: Timex.shift(Timex.now(), weeks: -4)})
      |> Api.create!()

      post_id = post1.id

      assert [%Post{id: ^post_id}] =
               Post
               |> Ash.Query.filter(approved_at > ago(2, :week))
               |> Api.read!()
    end
  end
end
