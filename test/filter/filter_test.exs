defmodule Ash.Test.Filter.FilterTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Test

  alias Ash.Filter
  alias Ash.Test.Domain, as: Domain

  require Ash.Query

  defmodule EmbeddedBio do
    @moduledoc false
    use Ash.Resource, data_layer: :embedded

    attributes do
      uuid_primary_key :id

      attribute :bio, :string do
        public?(true)
      end

      attribute :title, :string do
        public?(true)
      end
    end
  end

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :get_path_search do
        argument :input, :map

        filter expr(get_path(embedded_bio, [:title]) == get_path(^arg(:input), :title))
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string, public?: true
      attribute :embedded_bio, EmbeddedBio, public?: true
      attribute :private, :string
    end

    relationships do
      belongs_to :user, Ash.Test.Filter.FilterTest.User do
        public?(true)
      end
    end
  end

  defmodule User do
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
      attribute :name, :string, public?: true
      attribute :allow_second_author, :boolean, public?: true
      attribute :special, :boolean, public?: true
      attribute :roles, {:array, :atom}, public?: true
    end

    relationships do
      has_many :posts, Ash.Test.Filter.FilterTest.Post,
        destination_attribute: :author1_id,
        public?: true

      has_many :second_posts, Ash.Test.Filter.FilterTest.Post,
        destination_attribute: :author1_id,
        public?: true

      has_one :profile, Profile, destination_attribute: :user_id, public?: true
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
    end

    relationships do
      belongs_to :source_post, Ash.Test.Filter.FilterTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true

      belongs_to :destination_post, Ash.Test.Filter.FilterTest.Post,
        primary_key?: true,
        allow_nil?: false,
        public?: true
    end
  end

  defmodule Post do
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

      attribute :title, :string do
        public?(true)
      end

      attribute :contents, :string do
        public?(true)
      end

      attribute :points, :integer do
        public?(true)
      end

      attribute :approved_at, :datetime do
        public?(true)
      end

      attribute :category, :ci_string do
        public?(true)
      end
    end

    calculations do
      calculate :cool_titles, {:array, :string}, expr(["yo", "dawg"]) do
        public?(true)
      end
    end

    relationships do
      belongs_to :author1, User,
        destination_attribute: :id,
        source_attribute: :author1_id,
        public?: true

      belongs_to :special_author1, User,
        destination_attribute: :id,
        source_attribute: :author1_id,
        define_attribute?: false,
        filter: expr(special == true),
        public?: true

      belongs_to :author2, User,
        destination_attribute: :id,
        source_attribute: :author2_id,
        public?: true

      many_to_many :related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id,
        public?: true
    end
  end

  defmodule SoftDeletePost do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    resource do
      base_filter is_nil: :deleted_at
    end

    actions do
      default_accept :*
      defaults [:read, create: :*, update: :*]

      destroy :destroy do
        primary? true
        soft? true

        change set_attribute(:deleted_at, &DateTime.utc_now/0)
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :deleted_at, :utc_datetime do
        public?(true)
      end
    end
  end

  describe "in" do
    test "in can be done with references on both sides" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "dawg"})
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "lame"})
      |> Ash.create!()

      assert [_] =
               Post
               |> Ash.Query.filter(title in cool_titles)
               |> Ash.read!()
    end
  end

  describe "predicate optimization" do
    # Testing against the stringified query may be a bad idea, but its a quick win and we
    # can switch to actually checking the structure if this bites us
    test "equality simplifies to `in`" do
      stringified_query =
        Post
        |> Ash.Query.filter(title == "foo" or title == "bar")
        |> inspect()

      assert stringified_query =~ ~S(title in ["bar", "foo"])
    end

    test "in with equality simplifies to `in`" do
      stringified_query =
        Post
        |> Ash.Query.filter(title in ["foo", "bar", "baz"] or title == "bar")
        |> inspect()

      assert stringified_query =~ ~S(title in ["bar", "baz", "foo"])
    end

    test "in across ands in ors isn't optimized" do
      stringified_query =
        Post
        |> Ash.Query.filter(
          title == "foo" or
            (title == "bar" and points == 5) or
            (title == "baz" and points == 10)
        )
        |> inspect()

      assert stringified_query =~ ~S(title == "foo")
      assert stringified_query =~ ~S(title == "bar")
      assert stringified_query =~ ~S(title == "baz")
    end

    test "in with non-equality simplifies to `in`" do
      stringified_query =
        Post
        |> Ash.Query.filter(title in ["foo", "bar", "baz"] and title != "bar")
        |> inspect()

      assert stringified_query =~ ~S(title in ["baz", "foo"])
    end

    test "in with or-in simplifies to `in`" do
      stringified_query =
        Post
        |> Ash.Query.filter(title in ["foo", "bar"] or title in ["bar", "baz"])
        |> inspect()

      assert stringified_query =~ ~S(title in ["bar", "baz", "foo"])
    end

    test "in with and-in simplifies to `in` when multiple values overlap" do
      stringified_query =
        Post
        |> Ash.Query.filter(title in ["foo", "bar", "baz"] and title in ["bar", "baz", "bif"])
        |> inspect()

      assert stringified_query =~ ~S(title in ["bar", "baz"])
    end

    test "in with and-in simplifies to `eq` when one value overlaps" do
      stringified_query =
        Post
        |> Ash.Query.filter(title in ["foo", "bar"] and title in ["bar", "baz", "bif"])
        |> inspect()

      assert stringified_query =~ ~S(title == "bar")
    end
  end

  describe "simple attribute filters" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title1", contents: "contents1", points: 1})
        |> Ash.create!()
        |> strip_metadata()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2", contents: "contents2", points: 2})
        |> Ash.create!()
        |> strip_metadata()

      %{post1: post1, post2: post2}
    end

    test "single filter field", %{post1: post1} do
      assert [^post1] =
               Post
               |> Ash.Query.filter(title == ^post1.title)
               |> Ash.read!()
               |> strip_metadata()
    end

    test "multiple filter field matches", %{post1: post1} do
      assert [^post1] =
               Post
               |> Ash.Query.filter(title == ^post1.title and contents == ^post1.contents)
               |> Ash.read!()
               |> strip_metadata()
    end

    test "no field matches" do
      assert [] =
               Post
               |> Ash.Query.filter(title == "no match")
               |> Ash.read!()
    end

    test "no field matches single record, but each matches one record", %{
      post1: post1,
      post2: post2
    } do
      assert [] =
               Post
               |> Ash.Query.filter(title == ^post1.title and contents == ^post2.contents)
               |> Ash.read!()
    end

    test "less than works", %{
      post1: post1,
      post2: post2
    } do
      assert [^post1] =
               Post
               |> Ash.Query.filter(points < 2)
               |> Ash.read!()
               |> strip_metadata()

      assert [^post1, ^post2] =
               Post
               |> Ash.Query.filter(points < 3)
               |> Ash.Query.sort(points: :asc)
               |> Ash.read!()
               |> strip_metadata()
    end

    test "greater than works", %{
      post1: post1,
      post2: post2
    } do
      assert [^post2] =
               Post
               |> Ash.Query.filter(points > 1)
               |> Ash.read!()
               |> strip_metadata()

      assert [^post1, ^post2] =
               Post
               |> Ash.Query.filter(points > 0)
               |> Ash.Query.sort(points: :asc)
               |> Ash.read!()
               |> strip_metadata()
    end
  end

  describe "embedded filters" do
    setup do
      Profile
      |> Ash.Changeset.for_create(:create, %{embedded_bio: %{title: "Dr.", bio: "foo"}})
      |> Ash.create!()

      Profile
      |> Ash.Changeset.for_create(:create, %{
        embedded_bio: %{title: "Highlander", bio: "There can be only one"}
      })
      |> Ash.create!()

      :ok
    end

    test "simple equality filters work" do
      assert [%Profile{embedded_bio: %EmbeddedBio{title: "Dr."}}] =
               Profile
               |> Ash.Query.filter(embedded_bio[:title] == "Dr.")
               |> Ash.read!()
    end

    test "expressions work on accessed values" do
      assert [%Profile{embedded_bio: %EmbeddedBio{title: "Highlander"}}] =
               Profile
               |> Ash.Query.filter(contains(embedded_bio[:bio], "can be only one"))
               |> Ash.read!()
    end
  end

  describe "relationship filters" do
    setup do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title1", contents: "contents1", points: 1})
        |> Ash.create!()
        |> strip_metadata()

      post2 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title2", contents: "contents2", points: 2})
        |> Ash.create!()
        |> strip_metadata()

      post3 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title3", contents: "contents3", points: 3})
        |> Ash.Changeset.manage_relationship(:related_posts, [post1, post2],
          type: :append_and_remove
        )
        |> Ash.create!()
        |> strip_metadata()

      post4 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title4", contents: "contents4", points: 4})
        |> Ash.Changeset.manage_relationship(:related_posts, [post3], type: :append_and_remove)
        |> Ash.create!()
        |> strip_metadata()

      profile1 =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "dope"})
        |> Ash.create!()
        |> strip_metadata()

      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{name: "broseph"})
        |> Ash.Changeset.manage_relationship(:posts, [post1, post2], type: :append_and_remove)
        |> Ash.Changeset.manage_relationship(:profile, profile1, type: :append_and_remove)
        |> Ash.create!()
        |> strip_metadata()

      user2 =
        User
        |> Ash.Changeset.for_create(:create, %{name: "broseph", special: false})
        |> Ash.Changeset.manage_relationship(:posts, [post2], type: :append_and_remove)
        |> Ash.create!()
        |> strip_metadata()

      profile2 =
        Profile
        |> Ash.Changeset.for_create(:create, %{bio: "dope2"})
        |> Ash.Changeset.manage_relationship(:user, user2, type: :append_and_remove)
        |> Ash.create!()
        |> strip_metadata()

      %{
        post1: Ash.reload!(post1),
        post2: Ash.reload!(post2),
        post3: Ash.reload!(post3),
        post4: Ash.reload!(post4),
        profile1: Ash.reload!(profile1),
        user1: Ash.reload!(user1),
        user2: Ash.reload!(user2),
        profile2: Ash.reload!(profile2)
      }
    end

    test "filtering on a has_one relationship", %{profile2: profile2, user2: %{id: user2_id}} do
      assert [%{id: ^user2_id}] =
               User
               |> Ash.Query.filter(profile == ^profile2.id)
               |> Ash.read!()
    end

    test "filtering on a belongs_to relationship", %{profile1: %{id: id}, user1: user1} do
      assert [%{id: ^id}] =
               Profile
               |> Ash.Query.filter(user == ^user1.id)
               |> Ash.read!()
    end

    test "filtering on a has_many relationship", %{user2: %{id: user2_id}, post2: post2} do
      assert [%{id: ^user2_id}] =
               User
               |> Ash.Query.filter(posts == ^post2.id)
               |> Ash.read!()
    end

    test "filtering on a many_to_many relationship", %{post4: %{id: post4_id}, post3: post3} do
      assert [%{id: ^post4_id}] =
               Post
               |> Ash.Query.filter(related_posts == ^post3.id)
               |> Ash.read!()
    end

    test "relationship filters are honored when filtering on relationships", %{post2: post} do
      post = Ash.load!(post, [:special_author1, :author1])

      assert post.author1
      refute post.special_author1
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
      query = Ash.Query.filter(Post, points in [1, 2])

      assert Ash.Query.superset_of?(query, points == 1)
      assert Ash.Query.subset_of?(query, points in [1, 2, 3])
      assert Ash.Query.equivalent_to?(query, points == 1 or points == 2)
    end

    test "can detect a filter is not a subset based on a simplification" do
      filter = Filter.parse!(Post, points: [in: [1, 2]])

      candidate = Filter.parse!(Post, points: 3)

      refute Filter.strict_subset_of?(filter, candidate)
    end

    test "can detect that `not is_nil(field)` is the same as `field is_nil false`" do
      filter = Filter.parse!(Post, not: [is_nil: :points])
      candidate = Filter.parse!(Post, points: [is_nil: false])

      assert Filter.strict_subset_of?(filter, candidate)
      assert Filter.strict_subset_of?(candidate, filter)
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

    test "raises an error if the underlying parse returns an error" do
      filter = Filter.parse!(Post, points: 1)

      err =
        assert_raise(Ash.Error.Query.NoSuchField, fn ->
          Filter.add_to_filter!(filter, bad_field: "bad field")
        end)

      [error_context] = err.error_context
      assert error_context =~ "parsing addition of filter statement: [bad_field: \"bad field\"]"
      assert error_context =~ ", to resource: " <> (Post |> Module.split() |> Enum.join("."))
    end
  end

  describe "parse!" do
    test "raises an error if the statement is invalid" do
      err =
        assert_raise(Ash.Error.Invalid, fn ->
          Filter.parse!(Post, flarb: 1)
        end)

      [error_context] = err.error_context
      assert error_context =~ "parsing addition of filter statement: [flarb: 1]"
      assert error_context =~ ", to resource: " <> (Post |> Module.split() |> Enum.join("."))

      [inner_error] = err.errors
      [inner_error_context] = inner_error.error_context
      assert inner_error_context =~ "parsing addition of filter statement: [flarb: 1]"

      assert inner_error_context =~
               ", to resource: " <> (Post |> Module.split() |> Enum.join("."))
    end
  end

  describe "parse_input" do
    test "parse_input works when no private attributes are used" do
      Ash.Filter.parse_input!(Profile, bio: "foo")
    end

    test "parse_input fails when a private attribute is used" do
      Ash.Filter.parse!(Profile, private: "private")

      assert_raise(Ash.Error.Query.NoSuchField, fn ->
        Ash.Filter.parse_input!(Profile, private: "private")
      end)
    end
  end

  describe "base_filter" do
    test "resources that apply to the base filter are returned" do
      %{id: id} =
        SoftDeletePost
        |> Ash.Changeset.for_create(:create, %{})
        |> Ash.create!()

      assert [%{id: ^id}] = Ash.read!(SoftDeletePost)
    end

    test "resources that don't apply to the base filter are not returned" do
      SoftDeletePost
      |> Ash.Changeset.for_create(:create, %{})
      |> Ash.create!()
      |> Ash.destroy!()

      assert [] = Ash.read!(SoftDeletePost)
    end
  end

  describe "contains/2" do
    test "works for simple strings" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "foobar"})
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "bazbuz"})
      |> Ash.create!()

      assert [%{title: "foobar"}] =
               Post
               |> Ash.Query.filter(contains(title, "oba"))
               |> Ash.read!()
    end

    test "works for simple strings with a case insensitive search term" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "foobar"})
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "bazbuz"})
      |> Ash.create!()

      assert [%{title: "foobar"}] =
               Post
               |> Ash.Query.filter(contains(title, ^%Ash.CiString{string: "OBA"}))
               |> Ash.read!()
    end

    test "works for case insensitive strings" do
      Post
      |> Ash.Changeset.for_create(:create, %{category: "foobar"})
      |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{category: "bazbuz"})
      |> Ash.create!()

      assert [%{category: %Ash.CiString{string: "foobar"}}] =
               Post
               |> Ash.Query.filter(contains(category, "OBA"))
               |> Ash.read!()
    end
  end

  describe "length/1" do
    test "with an attribute" do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{roles: [:user]})
        |> Ash.create!()

      _user2 =
        User
        |> Ash.Changeset.for_create(:create, %{roles: []})
        |> Ash.create!()

      user1_id = user1.id

      assert [%User{id: ^user1_id}] =
               User
               |> Ash.Query.filter(length(roles) > 0)
               |> Ash.read!()
    end

    test "with an explicit list" do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{roles: [:user]})
        |> Ash.create!()

      user1_id = user1.id
      explicit_list = [:foo]

      assert [%User{id: ^user1_id}] =
               User
               |> Ash.Query.filter(length(^explicit_list) > 0)
               |> Ash.read!()

      assert [] =
               User
               |> Ash.Query.filter(length(^explicit_list) > 1)
               |> Ash.read!()
    end

    test "when nil" do
      user1 =
        User
        |> Ash.Changeset.for_create(:create, %{roles: [:user]})
        |> Ash.create!()

      _user2 =
        User
        |> Ash.Changeset.for_create(:create)
        |> Ash.create!()

      user1_id = user1.id

      assert [%User{id: ^user1_id}] =
               User
               |> Ash.Query.filter(length(roles || []) > 0)
               |> Ash.read!()
    end

    test "with bad input" do
      User
      |> Ash.Changeset.for_create(:create, %{name: "fred"})
      |> Ash.create!()

      assert_raise(Ash.Error.Unknown, fn ->
        User
        |> Ash.Query.filter(length(name) > 0)
        |> Ash.read!()
      end)
    end
  end

  describe "get_path/2" do
    test "it can be used by name" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{embedded_bio: %{title: "fred"}})
        |> Ash.create!()

      profile_id = profile.id

      Profile
      |> Ash.Changeset.for_create(:create, %{embedded_bio: %{title: "george"}})
      |> Ash.create!()

      assert [%{id: ^profile_id}] =
               Profile
               |> Ash.Query.filter(get_path(embedded_bio, :title) == "fred")
               |> Ash.read!()
    end

    test "it can be used with arguments" do
      profile =
        Profile
        |> Ash.Changeset.for_create(:create, %{embedded_bio: %{title: "fred"}})
        |> Ash.create!()

      profile_id = profile.id

      Profile
      |> Ash.Changeset.for_create(:create, %{embedded_bio: %{title: "george"}})
      |> Ash.create!()

      assert [%{id: ^profile_id}] =
               Profile
               |> Ash.Query.for_read(:get_path_search, %{input: %{title: "fred"}})
               |> Ash.read!()
    end
  end

  describe "calls in filters" do
    test "calls are evaluated and can be used in predicates" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title1", contents: "contents1", points: 2})
        |> Ash.create!()

      post_id = post1.id

      assert [%Post{id: ^post_id}] =
               Post
               |> Ash.Query.filter(points + 1 == 3)
               |> Ash.read!()
    end

    test "function calls are evaluated properly" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title1",
          approved_at: DateTime.new!(Date.utc_today() |> Date.add(-7), Time.utc_now())
        })
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "title1",
        approved_at: DateTime.new!(Date.utc_today() |> Date.add(-7 * 4), Time.utc_now())
      })
      |> Ash.create!()

      post_id = post1.id

      assert [%Post{id: ^post_id}] =
               Post
               |> Ash.Query.filter(approved_at > ago(2, :week))
               |> Ash.read!()
    end

    test "now() evaluates to the current datetime" do
      post1 =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title1",
          approved_at: DateTime.new!(Date.utc_today() |> Date.add(7), Time.utc_now())
        })
        |> Ash.create!()

      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "title1",
        approved_at: DateTime.new!(Date.utc_today() |> Date.add(-7), Time.utc_now())
      })
      |> Ash.create!()

      post_id = post1.id

      assert [%Post{id: ^post_id}] =
               Post
               |> Ash.Query.filter(approved_at > now())
               |> Ash.read!()
    end
  end

  test "using tuple instead of keyword list does not raise an error" do
    Post
    |> Ash.Query.filter(id: {:in, [Ash.UUID.generate()]})
    |> Ash.read!()
  end

  test "parsing input with embedded references works" do
    Profile
    |> Ash.Changeset.for_create(:create, %{
      embedded_bio: %{title: "Mr."}
    })
    |> Ash.create!()

    Profile
    |> Ash.Changeset.for_create(:create, %{
      embedded_bio: %{title: "Dr."}
    })
    |> Ash.create!()

    assert [%{embedded_bio: %{title: "Dr."}}] =
             Profile
             |> Ash.Query.filter_input(%{embedded_bio: %{at_path: [:title], eq: "Dr."}})
             |> Ash.read!()
  end
end
