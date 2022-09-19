defmodule Ash.Test.Filter.FilterInteractionTest do
  use ExUnit.Case, async: false

  import Ash.Changeset
  import ExUnit.CaptureLog
  import Ash.Test

  alias Ash.DataLayer.Mnesia

  require Ash.Query

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:bio, :string)
    end

    relationships do
      belongs_to(:user, Ash.Test.Filter.FilterInteractionTest.User)
    end
  end

  defmodule User do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string)
      attribute(:allow_second_author, :boolean)
    end

    relationships do
      has_many(:posts, Ash.Test.Filter.FilterInteractionTest.Post,
        destination_attribute: :author_id
      )

      has_many(:second_posts, Ash.Test.Filter.FilterInteractionTest.Post,
        destination_attribute: :author_id
      )

      has_one(:profile, Profile, destination_attribute: :user_id)
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    relationships do
      belongs_to(:source_post, Ash.Test.Filter.FilterInteractionTest.Post,
        primary_key?: true,
        allow_nil?: false
      )

      belongs_to(:destination_post, Ash.Test.Filter.FilterInteractionTest.Post,
        primary_key?: true,
        allow_nil?: false
      )
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string)
      attribute(:contents, :string)
      attribute(:points, :integer)
    end

    relationships do
      belongs_to(:author, User,
        destination_attribute: :id,
        source_attribute: :author_id
      )

      many_to_many(:related_posts, __MODULE__,
        through: PostLink,
        source_attribute_on_join_resource: :source_post_id,
        destination_attribute_on_join_resource: :destination_post_id
      )
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Post)
      entry(User)
      entry(Profile)
      entry(PostLink)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  setup do
    capture_log(fn ->
      Mnesia.start(Api)
    end)

    on_exit(fn ->
      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)
  end

  test "mnesia data layer sanity test" do
    post =
      Post
      |> new(%{title: "best"})
      |> Api.create!()
      |> strip_metadata()

    assert [^post] = strip_metadata(Api.read!(Post))

    post |> new(%{title: "worst"}) |> Api.update!()

    new_post = %{post | title: "worst"}

    assert [^new_post] = strip_metadata(Api.read!(Post))

    Api.destroy!(post)

    assert [] = Api.read!(Post)
  end

  describe "cross data layer filtering" do
    test "it properly filters with a simple filter" do
      author =
        User
        |> new(%{name: "best author"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "best"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post1_id = post1.id

      Post
      |> new(%{title: "worst"})
      |> Api.create!()

      query =
        Post
        |> Ash.Query.filter(author.name == "best author")

      assert [%{id: ^post1_id}] = Api.read!(query)
    end

    test "parallelizable filtering of related resources with a data layer that cannot join" do
      post2 =
        Post
        |> new(%{title: "two"})
        |> Api.create!()

      Post
      |> new(%{title: "three"})
      |> Api.create!()

      post1 =
        Post
        |> new(%{title: "one"})
        |> manage_relationship(:related_posts, [post2], type: :append_and_remove)
        |> Api.create!()

      query =
        Post
        |> Ash.Query.filter(related_posts.title == "two")

      post1_id = post1.id

      assert [%{id: ^post1_id}] = Api.read!(query)
    end

    test "parallelizable filter with filtered loads" do
      post2 =
        Post
        |> new(%{title: "two"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "three"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "one"})
        |> manage_relationship(:related_posts, [post2, post3], type: :append_and_remove)
        |> Api.create!()

      post2
      |> Api.load!(:related_posts)

      posts_query =
        Post
        |> Ash.Query.filter(title == "three")

      query =
        Post
        |> Ash.Query.filter(related_posts.title == "two")
        |> Ash.Query.load(related_posts: posts_query)

      post1_id = post1.id

      post3_id = post3.id

      assert [%{id: ^post1_id, related_posts: [%{id: ^post3_id}]}] = Api.read!(query)
    end

    test "exists/2 in the same data layer" do
      post2 =
        Post
        |> new(%{title: "two"})
        |> Api.create!()

      post3 =
        Post
        |> new(%{title: "three"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "one"})
        |> manage_relationship(:related_posts, [post2, post3], type: :append_and_remove)
        |> Api.create!()

      Post
      |> new(%{title: "four"})
      |> manage_relationship(:related_posts, [post3], type: :append_and_remove)
      |> Api.create!()

      post2
      |> Api.load!(:related_posts)

      query =
        Post
        |> Ash.Query.filter(exists(related_posts, title == "two"))

      post1_id = post1.id

      assert [%{id: ^post1_id}] = Api.read!(query)
    end

    test "exists/2 across data layers" do
      author =
        User
        |> new(%{name: "best author"})
        |> Api.create!()

      author2 =
        User
        |> new(%{name: "worst author"})
        |> Api.create!()

      post1 =
        Post
        |> new(%{title: "best"})
        |> manage_relationship(:author, author, type: :append_and_remove)
        |> Api.create!()

      post1_id = post1.id

      Post
      |> new(%{title: "worst"})
      |> manage_relationship(:author, author2, type: :append_and_remove)
      |> Api.create!()

      query = Ash.Query.filter(Post, exists(author, contains(name, "best")))

      assert [%{id: ^post1_id}] = Api.read!(query)
    end
  end
end
