defmodule Ash.Test.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Changeset
  require Ash.Query

  defmodule Profile do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :bio, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.DestroyTest.Author
    end
  end

  defmodule ManualDestroyAuthor do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _changeset, data ->
        Ash.Test.Actions.DestroyTest.Api.destroy!(data)

        {:ok, data}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]

      destroy :manual do
        accept []
        manual? true
        change ManualDestroyAuthor
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string
    end

    relationships do
      has_one :profile, Profile, destination_field: :author_id

      has_many :posts, Ash.Test.Actions.DestroyTest.Post, destination_field: :author_id
    end
  end

  defmodule PostDefaults do
    @moduledoc false
    def garbage2, do: "garbage2"
    def garbage3, do: "garbage3"
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets

    ets do
      private?(true)
    end

    actions do
      defaults [:create, :read, :update, :destroy]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string
      attribute :contents, :string
      attribute :tag, :string, default: "garbage"
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3, []}
    end

    relationships do
      belongs_to :author, Author
    end
  end

  defmodule Registry do
    @moduledoc false
    use Ash.Registry

    entries do
      entry(Author)
      entry(Post)
      entry(Profile)
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      registry Registry
    end
  end

  describe "simple destroy" do
    test "allows destroying a record" do
      post =
        Post
        |> new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert Api.destroy!(post) == :ok

      refute Api.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "returns the record if requested" do
      post =
        Post
        |> new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}} = Api.destroy(post, return_destroyed?: true)

      refute Api.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "returns the record and notifications if requested" do
      post =
        Post
        |> new(%{title: "foo", contents: "bar"})
        |> Api.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}, [_]} =
               Api.destroy(post, return_destroyed?: true, return_notifications?: true)

      refute Api.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "the destroy does not happen if it is unauthorized" do
      author =
        Author
        |> new(%{name: "foobar"})
        |> Api.create!()

      start_supervised({Ash.Test.Authorizer, strict_check: :continue, check: :forbidden})

      assert_raise(Ash.Error.Forbidden, fn ->
        Api.destroy!(author, authorize?: true)
      end)

      assert Api.get!(Author, author.id)
    end
  end

  describe "manual destroy" do
    test "allows destroying a record" do
      author =
        Author
        |> new(%{name: "foo"})
        |> Api.create!()

      assert Api.destroy!(author, action: :manual) == :ok

      refute Api.read_one!(Ash.Query.filter(Author, id == ^author.id))
    end
  end
end
