defmodule Ash.Test.Actions.DestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

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
      destroy :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :bio, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.DestroyTest.Author
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, authorizers: [Ash.Test.Authorizer]

    ets do
      private?(true)
    end

    actions do
      read :default
      create :default
      update :default
      destroy :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
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
      read :default
      create :default
      update :default
      destroy :default
    end

    attributes do
      attribute :id, :uuid, primary_key?: true, default: &Ecto.UUID.generate/0
      attribute :title, :string
      attribute :contents, :string
      attribute :tag, :string, default: {:constant, "garbage"}
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3, []}
    end

    relationships do
      belongs_to :author, Author
    end
  end

  defmodule Api do
    @moduledoc false
    use Ash.Api

    resources do
      resource(Author)
      resource(Post)
      resource(Profile)
    end
  end

  import Ash.Changeset

  describe "simple destroy" do
    test "allows destroying a record" do
      post =
        Post
        |> create(%{title: "foo", contents: "bar"})
        |> Api.create!()

      assert Api.destroy!(post) == :ok

      refute Api.get!(Post, post.id)
    end

    test "the destroy does not happen if it is unauthorized" do
      author =
        Author
        |> create(%{name: "foobar"})
        |> Api.create!()

      assert_raise(Ash.Error.Forbidden, fn ->
        Api.destroy!(author, authorize?: true)
      end)

      assert Api.get!(Author, author.id)
    end
  end
end
