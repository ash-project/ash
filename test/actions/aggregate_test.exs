defmodule Ash.Test.Actions.AggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.AnyApi, as: Api

  defmodule Comment do
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id

      attribute :public, :boolean do
        default false
      end

      attribute :thing, :string

      attribute :thing2, :decimal

      attribute :thing3, :integer
    end

    relationships do
      belongs_to :post, Ash.Test.Actions.AggregateTest.Post do
        attribute_writable? true
      end
    end

    policies do
      policy always() do
        authorize_if expr(public == true)
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      api: Api,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
      read :unpublic

      read :with_foo do
        prepare build(filter: [foo: true])
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string

      attribute :public, :boolean do
        default false
      end

      attribute :tenant, :string
      attribute :thing, :string
      attribute :foo, :boolean, default: false
    end

    multitenancy do
      global? true
      strategy :attribute
      attribute :tenant
    end

    aggregates do
      count :count_of_comments, :comments

      count :count_of_posts, [:comments, :post]

      count :count_of_comment_posts_with_matching_things, [:comments, :post] do
        join_filter(:comments, expr(parent(thing) == thing))
      end

      count :count_of_comments_unauthorized, :comments do
        authorize? false
      end

      min :min_of_thing2, :comments, :thing2 do
        authorize? false
      end

      max :max_of_thing2, :comments, :thing2 do
        authorize? false
      end

      avg :average_of_thing2, :comments, :thing2 do
        authorize? false
      end

      min :min_of_thing3, :comments, :thing3 do
        authorize? false
      end

      max :max_of_thing3, :comments, :thing3 do
        authorize? false
      end

      avg :average_of_thing3, :comments, :thing3 do
        authorize? false
      end
    end

    relationships do
      has_many :comments, Comment
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

  describe "Api.aggregate" do
    test "allows counting records" do
      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!(authorize?: false)

      assert %{count: 1} = Api.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!(authorize?: false)

      assert %{count: 2} = Api.aggregate!(Post, {:count, :count}, authorize?: false)
    end

    test "honors tenant" do
      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "foo"})
      |> Api.create!(authorize?: false)

      assert %{count: 1} =
               Api.aggregate!(Post, {:count, :count}, tenant: "foo", authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "foo"})
      |> Api.create!(authorize?: false)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", tenant: "bar"})
      |> Api.create!(authorize?: false)

      assert %{count: 2} =
               Api.aggregate!(Post, {:count, :count}, tenant: "foo", authorize?: false)

      assert %{count: 3} = Api.aggregate!(Post, {:count, :count}, authorize?: false)
    end

    test "runs authorization" do
      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)

      assert 0 = Api.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!(authorize?: false)

      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 0 = Api.count!(Post, authorize?: true)

      assert %{count: 1} =
               Post
               |> Ash.Query.for_read(:unpublic)
               |> Api.aggregate!({:count, :count}, actor: nil)

      assert %{count: 1} =
               Api.aggregate!(Post, {:count, :count}, actor: nil, action: :unpublic)

      assert 1 = Api.count!(Post, actor: nil, action: :unpublic, authorize?: true)
      assert 0 = Api.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
      |> Api.create!(authorize?: false)

      assert %{count: 1} = Api.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 1 = Api.count!(Post, authorize?: true)
    end

    test "use custom actions from the query" do
      Post
      |> Ash.Changeset.for_create(:create, %{title: "without foo", foo: false})
      |> Api.create!()

      assert false ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Api.exists?()

      Post
      |> Ash.Changeset.for_create(:create, %{title: "with foo", foo: true})
      |> Api.create!()

      assert 1 ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Api.count!()

      assert ["with foo"] ==
               Post
               |> Ash.Query.for_read(:with_foo)
               |> Api.list!(:title)
    end
  end

  describe "aggregate loading" do
    test "loading aggregates can be authorized or not" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
        |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: true})
      |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: false})
      |> Api.create!(authorize?: false)

      assert %{count_of_comments: 2, count_of_comments_unauthorized: 2} =
               Api.load!(post, [:count_of_comments, :count_of_comments_unauthorized],
                 authorize?: false
               )

      assert %{count_of_comments: 1, count_of_comments_unauthorized: 2} =
               Api.load!(post, [:count_of_comments, :count_of_comments_unauthorized],
                 authorize?: true
               )
    end

    test "join filters are applied" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{
          title: "title",
          public: true,
          thing: "not the same"
        })
        |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match"
      })
      |> Api.create!(authorize?: false)

      assert Api.load!(post, :count_of_comment_posts_with_matching_things, authorize?: false).count_of_comment_posts_with_matching_things ==
               0

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "not the same"
      })
      |> Api.create!(authorize?: false)

      assert Api.load!(post, :count_of_comment_posts_with_matching_things, authorize?: false).count_of_comment_posts_with_matching_things ==
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
        |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: 10,
        thing3: 100
      })
      |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: 20,
        thing3: 200
      })
      |> Api.create!(authorize?: false)

      Comment
      |> Ash.Changeset.for_create(:create, %{
        post_id: post.id,
        public: true,
        thing: "doesnt match",
        thing2: nil,
        thing3: nil
      })
      |> Api.create!(authorize?: false)

      assert Decimal.eq?(
               Api.load!(post, :min_of_thing2, authorize?: false).min_of_thing2,
               Decimal.new(10)
             )

      assert Decimal.eq?(
               Api.load!(post, :max_of_thing2, authorize?: false).max_of_thing2,
               Decimal.new(20)
             )

      assert Decimal.eq?(
               Api.load!(post, :average_of_thing2, authorize?: false).average_of_thing2,
               Decimal.new(15)
             )

      assert Api.load!(post, :min_of_thing3, authorize?: false).min_of_thing3 == 100
      assert Api.load!(post, :max_of_thing3, authorize?: false).max_of_thing3 == 200
      assert Api.load!(post, :average_of_thing3, authorize?: false).average_of_thing3 == 150
    end
  end
end
