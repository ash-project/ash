defmodule Ash.Test.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.Domain, as: Domain

  defmodule Profile do
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

      attribute :bio, :string do
        public?(true)
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.DestroyTest.Author do
        public?(true)
      end
    end
  end

  defmodule ManualDestroyAuthor do
    @moduledoc false
    use Ash.Resource.Change

    def change(changeset, _, _) do
      Ash.Changeset.after_action(changeset, fn _changeset, data ->
        Ash.destroy!(data)

        {:ok, data}
      end)
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      destroy :manual do
        accept []

        manual fn changeset, _ ->
          Ash.destroy(changeset.data, return_destroyed?: true)
        end
      end
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      has_one :profile, Profile, destination_attribute: :author_id, public?: true

      has_many :posts, Ash.Test.Actions.DestroyTest.Post,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule PostDefaults do
    @moduledoc false
    def garbage2, do: "garbage2"
    def garbage3, do: "garbage3"
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
      attribute :title, :string, public?: true
      attribute :contents, :string, public?: true
      attribute :tag, :string, default: "garbage", public?: true
      attribute :tag2, :string, default: &PostDefaults.garbage2/0, public?: true
      attribute :tag3, :string, default: {PostDefaults, :garbage3, []}, public?: true
    end

    relationships do
      belongs_to :author, Author do
        public?(true)
      end
    end
  end

  describe "simple destroy" do
    test "allows destroying a record" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      assert Ash.destroy!(post) == :ok

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "returns the record if requested" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}} = Ash.destroy(post, return_destroyed?: true)

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "returns the record and notifications if requested" do
      post =
        Post
        |> Ash.Changeset.for_create(:create, %{title: "foo", contents: "bar"})
        |> Ash.create!()

      post_id = post.id

      assert {:ok, %{id: ^post_id}, [_]} =
               Ash.destroy(post, return_destroyed?: true, return_notifications?: true)

      refute Ash.read_one!(Ash.Query.filter(Post, id == ^post.id))
    end

    test "the destroy does not happen if it is unauthorized" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foobar"})
        |> Ash.create!(authorize?: false)

      start_supervised({Ash.Test.Authorizer, strict_check: :continue, check: :forbidden})

      assert_raise(Ash.Error.Forbidden, fn ->
        Ash.destroy!(author, authorize?: true)
      end)

      assert Ash.get!(Author, author.id, authorize?: false)
    end
  end

  describe "manual destroy" do
    test "allows destroying a record" do
      author =
        Author
        |> Ash.Changeset.for_create(:create, %{name: "foo"})
        |> Ash.create!()

      assert Ash.destroy!(author, action: :manual) == :ok

      refute Ash.read_one!(Ash.Query.filter(Author, id == ^author.id))
    end
  end
end
