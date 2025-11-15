# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.AggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Comment.ReadActionModifyQuery do
    def modify(_ash_query, _ecto_query) do
      raise "Should raise!"
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      read :with_modify_query do
        modify_query {Comment.ReadActionModifyQuery, :modify, []}
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :public, :boolean do
        public?(true)
        default false
      end

      attribute :thing, :string do
        public?(true)
      end

      attribute :thing2, :decimal do
        public?(true)
      end

      attribute :thing3, :integer do
        public?(true)
      end

      create_timestamp :created_at
    end

    calculations do
      calculate :doubled_thing3, :integer, expr(thing3 * 2) do
        public?(true)
      end
    end

    relationships do
      belongs_to :post, Ash.Test.Actions.AggregateTest.Post do
        public?(true)
      end
    end

    policies do
      policy do
        condition(always())
        authorize_if expr(public == true)
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]
      read :unpublic

      read :with_foo do
        prepare build(filter: [foo: true])
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :title, :string do
        public?(true)
      end

      attribute :views, :integer

      attribute :public, :boolean do
        public?(true)
        default false
      end

      attribute :tenant, :string do
        public?(true)
      end

      attribute :thing, :string do
        public?(true)
      end

      attribute :foo, :boolean, default: false, public?: true
    end

    multitenancy do
      global? true
      strategy :attribute
      attribute :tenant
    end

    aggregates do
      count :count_of_comments, :comments do
        public? false
      end

      count :count_of_comments_matching_actor, :comments do
        public? false
        filter expr(thing == ^actor(:name))
      end

      count :count_of_posts, [:comments, :post] do
        public? false
      end

      count :count_of_comment_posts_with_matching_things, [:comments, :post] do
        public? false
        join_filter(:comments, expr(parent(thing) == thing))
      end

      count :count_of_comments_unauthorized, :comments do
        public? true
        authorize? false
      end

      count :count_of_comments_modify_query, :comments do
        public? true
        read_action :with_modify_query
      end

      min :min_of_thing2, :comments, :thing2 do
        public? true
        authorize? false
      end

      max :max_of_thing2, :comments, :thing2 do
        public? true
        authorize? false
      end

      avg :average_of_thing2, :comments, :thing2 do
        public? true
        authorize? false
      end

      min :min_of_thing3, :comments, :thing3 do
        public? true
        authorize? false
      end

      max :max_of_thing3, :comments, :thing3 do
        public? true
        authorize? false
      end

      avg :average_of_thing3, :comments, :thing3 do
        public? true
        authorize? false
      end

      sum :sum_of_doubled_thing3, :comments, :doubled_thing3 do
        public? true
        authorize? false
      end
    end

    relationships do
      has_many :comments, Comment do
        public?(true)
      end

      has_many :latest_comments, Comment do
        limit(2)
        sort created_at: :desc
        public?(true)
      end
    end

    policies do
      policy action(:read) do
        authorize_if expr(public == true)
      end

      policy action(:unpublic) do
        authorize_if expr(public == false)
      end
    end
  end

  describe "Ash.aggregate" do
    test "allows counting records" do
      assert %{count: 0} = Ash.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Ash.create!(authorize?: false)

      assert %{count: 1} = Ash.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Ash.create!(authorize?: false)

      assert %{count: 2} = Ash.aggregate!(Post, {:count, :count}, authorize?: false)
    end

    test "honors tenant" do
      assert %{count: 0} = Ash.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "foo"})
      |> Ash.create!(authorize?: false)

      assert %{count: 1} =
               Ash.aggregate!(Post, {:count, :count}, tenant: "foo", authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "foo"})
      |> Ash.create!(authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "bar"})
      |> Ash.create!(authorize?: false)

      assert %{count: 2} =
               Ash.aggregate!(Post, {:count, :count}, tenant: "foo", authorize?: false)

      assert %{count: 3} = Ash.aggregate!(Post, {:count, :count}, authorize?: false)
    end

    test "counts should match read + Enum count" do
      # create 10 unpublished posts
      Enum.each(1..10, fn _ ->
        Ash.Seed.seed!(Post, %{public: false})
      end)

      # create 1 public post
      Ash.Seed.seed!(Post, %{public: true, views: 0})

      query =
        Post
        |> Ash.Query.for_read(:read)
        |> Ash.Query.filter(public == true)
        |> Ash.Query.sort(views: :desc)
        |> Ash.Query.limit(1)

      direct_count = Ash.count!(query)
      enum_count = query |> Ash.read!() |> Enum.count()
      assert direct_count == enum_count
    end

    test "runs authorization" do
      assert %{count: 0} = Ash.aggregate!(Post, {:count, :count}, authorize?: true)

      assert 0 = Ash.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Ash.create!(authorize?: false)

      assert %{count: 0} = Ash.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 0 = Ash.count!(Post, authorize?: true)

      assert %{count: 1} =
               Post
               |> Ash.Query.for_read(:unpublic)
               |> Ash.aggregate!({:count, :count}, actor: nil)

      assert %{count: 1} =
               Ash.aggregate!(Post, {:count, :count}, actor: nil, action: :unpublic)

      assert 1 = Ash.count!(Post, actor: nil, action: :unpublic, authorize?: true)
      assert 0 = Ash.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
      |> Ash.create!(authorize?: false)

      assert %{count: 1} = Ash.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 1 = Ash.count!(Post, authorize?: true)
    end

    test "use custom actions from the query" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "without foo", foo: false})
      |> Ash.create!(authorize?: false)

      assert false ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Ash.exists?(authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "with foo", foo: true})
      |> Ash.create!(authorize?: false)

      assert 1 ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Ash.count!(authorize?: false)

      assert ["with foo"] ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Ash.list!(:title, authorize?: false)
    end

    test "returns error for invalid input" do
      assert {:error, %Ash.Error.Invalid{errors: [%Ash.Error.Invalid.NoSuchInput{input: :bar}]}} =
               Post
               |> Ash.Query.for_read(:with_foo, %{bar: "no such input"})
               |> Ash.exists(authorize?: false)
    end
  end

  describe "aggregate loading" do
    test "the actor can be used in aggregate filters" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
        |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, thing: "foobar", public: true})
      |> Ash.create!(authorize?: false)

      count =
        Ash.load!(post, :count_of_comments_matching_actor, actor: %{name: "foobar"})
        |> Map.get(:count_of_comments_matching_actor)

      assert count == 1
    end

    test "loading aggregates can be authorized or not" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
        |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: true})
      |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: false})
      |> Ash.create!(authorize?: false)

      assert %{count_of_comments: 2, count_of_comments_unauthorized: 2} =
               Ash.load!(post, [:count_of_comments, :count_of_comments_unauthorized],
                 authorize?: false
               )

      assert %{count_of_comments: 1, count_of_comments_unauthorized: 2} =
               Ash.load!(post, [:count_of_comments, :count_of_comments_unauthorized],
                 authorize?: true
               )
    end

    test "loads relationship with limit" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
        |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: true})
      |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: false})
      |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: false})
      |> Ash.create!(authorize?: false)

      assert %{latest_comments: latest_comments} =
               Ash.load!(post, [:latest_comments], authorize?: false)

      assert 2 == Enum.count(latest_comments)
    end

    test "join filters are applied" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title",
          public: true,
          thing: "not the same"
        })
        |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match"
      })
      |> Ash.create!(authorize?: false)

      assert Ash.load!(post, :count_of_comment_posts_with_matching_things, authorize?: false).count_of_comment_posts_with_matching_things ==
               0

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "not the same"
      })
      |> Ash.create!(authorize?: false)

      assert Ash.load!(post, :count_of_comment_posts_with_matching_things, authorize?: false).count_of_comment_posts_with_matching_things ==
               1
    end

    test "aggregations on decimal fields succeed" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title",
          public: true,
          thing: "not the same"
        })
        |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: 10,
        thing3: 100
      })
      |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: 20,
        thing3: 200
      })
      |> Ash.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: nil,
        thing3: nil
      })
      |> Ash.create!(authorize?: false)

      assert Decimal.eq?(
               Ash.load!(post, :min_of_thing2, authorize?: false).min_of_thing2,
               Decimal.new(10)
             )

      assert Decimal.eq?(
               Ash.load!(post, :max_of_thing2, authorize?: false).max_of_thing2,
               Decimal.new(20)
             )

      assert Decimal.eq?(
               Ash.load!(post, :average_of_thing2, authorize?: false).average_of_thing2,
               Decimal.new(15)
             )

      assert Ash.load!(post, :min_of_thing3, authorize?: false).min_of_thing3 == 100
      assert Ash.load!(post, :max_of_thing3, authorize?: false).max_of_thing3 == 200
      assert Ash.load!(post, :average_of_thing3, authorize?: false).average_of_thing3 == 150
    end

    test "aggregations with read_action should apply modify_query" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title",
          public: true,
          thing: "not the same"
        })
        |> Ash.create!(authorize?: false)

      assert_raise Ash.Error.Unknown, ~r/Should raise!/, fn ->
        Ash.read!(Comment, action: :with_modify_query, authorize?: false)
      end

      assert_raise Ash.Error.Unknown, ~r/Should raise!/, fn ->
        Ash.load!(post, :count_of_comments_modify_query, authorize?: false)
      end
    end

    test "aggregates can reference calculations" do
      post = Post |> Ash.create!(%{public: true}, authorize?: false)

      Comment
      |> Ash.create!(%{post_id: post.id, public: true, thing3: 5}, authorize?: false)

      Comment
      |> Ash.create!(%{post_id: post.id, public: true, thing3: 10}, authorize?: false)

      Comment
      |> Ash.create!(%{post_id: post.id, public: true, thing3: 3}, authorize?: false)

      post = Ash.load!(post, :sum_of_doubled_thing3, authorize?: false)

      assert post.sum_of_doubled_thing3 == 36
    end
  end
end
