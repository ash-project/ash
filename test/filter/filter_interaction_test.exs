defmodule Ash.Test.Filter.FilterInteractionTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Ash.DataLayer.Mnesia

  require Ash.Query

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
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

    ets do
      private?(true)
    end

    actions do
      read(:default)
      create(:default)
      update(:default)
    end

    attributes do
      uuid_primary_key :id
      attribute(:name, :string)
      attribute(:allow_second_author, :boolean)
    end

    relationships do
      has_many(:posts, Ash.Test.Filter.FilterInteractionTest.Post, destination_field: :author_id)

      has_many(:second_posts, Ash.Test.Filter.FilterInteractionTest.Post,
        destination_field: :author_id
      )

      has_one(:profile, Profile, destination_field: :user_id)
    end
  end

  defmodule PostLink do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      read(:default)

      create(:default)
      update(:default)
    end

    relationships do
      belongs_to(:source_post, Ash.Test.Filter.FilterInteractionTest.Post, primary_key?: true)

      belongs_to(:destination_post, Ash.Test.Filter.FilterInteractionTest.Post, primary_key?: true)
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Mnesia

    actions do
      read(:default)

      create(:default)

      update(:default)

      destroy(:default)
    end

    attributes do
      uuid_primary_key :id
      attribute(:title, :string)
      attribute(:contents, :string)
      attribute(:points, :integer)
    end

    relationships do
      belongs_to(:author, User,
        destination_field: :id,
        source_field: :author_id
      )

      many_to_many(:related_posts, __MODULE__,
        through: PostLink,
        source_field_on_join_table: :source_post_id,
        destination_field_on_join_table: :destination_post_id
      )
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Post)
      resource(User)
      resource(Profile)
      resource(PostLink)
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

  import Ash.Changeset

  test "mnesia data layer sanity test" do
    post =
      Post
      |> new(%{title: "best"})
      |> Api.create!()

    assert [^post] = Api.read!(Post)

    post |> new(%{title: "worst"}) |> Api.update!()

    new_post = %{post | title: "worst"}

    assert [^new_post] = Api.read!(Post)

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
        |> replace_relationship(:author, author)
        |> Api.create!()

      Post
      |> new(%{title: "worst"})
      |> Api.create!()

      post1 = Api.reload!(post1)

      query =
        Post
        |> Ash.Query.filter(author.name == "best author")

      assert [^post1] = Api.read!(query)
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
        |> replace_relationship(:related_posts, [post2])
        |> Api.create!()

      query =
        Post
        |> Ash.Query.filter(related_posts.title == "two")

      post1 = Api.reload!(post1)

      assert [^post1] = Api.read!(query)
    end

    test "parallelizable filter with filtered side loads" do
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
        |> replace_relationship(:related_posts, [post2, post3])
        |> Api.create!()

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
  end
end
