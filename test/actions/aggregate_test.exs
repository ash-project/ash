defmodule Ash.Test.Actions.AggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  defmodule Comment do
    use Ash.Resource,
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
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
      read :unpublic
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string

      attribute :public, :boolean do
        default false
      end
    end

    aggregates do
      count :count_of_comments, :comments

      count :count_of_comments_unauthorized, :comments do
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

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
      entry(Comment)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "Api.aggregate" do
    test "allows counting records" do
      assert %{count: 0} = Api.aggregate!(Post, {:count, :count})

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!()

      assert %{count: 1} = Api.aggregate!(Post, {:count, :count})

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!()

      assert %{count: 2} = Api.aggregate!(Post, {:count, :count})
    end

    test "runs authorization" do
      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)

      assert 0 = Api.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title"})
      |> Api.create!()

      assert %{count: 0} = Api.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 0 = Api.count!(Post, authorize?: true)

      assert %{count: 1} =
               Post
               |> Ash.Query.for_read(:unpublic)
               |> Api.aggregate!({:count, :count}, actor: nil)

      assert %{count: 1} =
               Api.aggregate!(Post, {:count, :count}, actor: nil, action: :unpublic)

      assert 1 = Api.count!(Post, actor: nil, action: :unpublic)
      assert 0 = Api.count!(Post, authorize?: true)

      Post
      |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
      |> Api.create!()

      assert %{count: 1} = Api.aggregate!(Post, {:count, :count}, authorize?: true)
      assert 1 = Api.count!(Post, authorize?: true)
    end
  end

  describe "aggregate loading" do
    test "loading aggregates can be authorized or not" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "title", public: true})
        |> Api.create!()

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: true})
      |> Api.create!()

      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, public: false})
      |> Api.create!()

      assert %{count_of_comments: 2, count_of_comments_unauthorized: 2} =
               Api.load!(post, [:count_of_comments, :count_of_comments_unauthorized])

      assert %{count_of_comments: 1, count_of_comments_unauthorized: 2} =
               Api.load!(post, [:count_of_comments, :count_of_comments_unauthorized],
                 authorize?: true
               )
    end
  end
end
