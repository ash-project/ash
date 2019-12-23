defmodule Ash.Test.Actions.DestroyTest do
  use ExUnit.Case, async: true

  defmodule Profile do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
      update :default
      destroy :default
    end

    attributes do
      attribute :bio, :string
    end

    relationships do
      belongs_to :author, Ash.Test.Actions.CreateTest.Author
    end
  end

  defmodule Author do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
      update :default
      destroy :default
    end

    attributes do
      attribute :name, :string
    end

    relationships do
      has_one :profile, Profile

      has_many :posts, Ash.Test.Actions.CreateTest.Post
    end
  end

  defmodule PostDefaults do
    def garbage2(), do: "garbage2"
    def garbage3(), do: "garbage3"
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
      update :default
      destroy :default
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
      attribute :tag, :string, default: {:constant, "garbage"}
      attribute :tag2, :string, default: &PostDefaults.garbage2/0
      attribute :tag3, :string, default: {PostDefaults, :garbage3}
    end

    relationships do
      belongs_to :author, Author
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Author, Post, Profile]
  end

  describe "simple destroy" do
    test "allows destroying a record" do
      post = Api.create!(Post, attributes: %{title: "foo", contents: "bar"})

      assert Api.destroy!(post) == post

      refute Api.get!(Post, post.id)
    end
  end
end
