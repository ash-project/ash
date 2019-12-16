defmodule Ash.Test.Authorization.AuthorizationTest do
  use ExUnit.Case, async: true

  defmodule Author do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :name, :string
    end

    relationships do
      many_to_many :posts, Ash.Test.Authorization.AuthorizationTest.Post,
        through: Ash.Test.Authorization.AuthorizationTest.AuthorPost
    end
  end

  defmodule AuthorPost do
    use Ash.Resource, name: "author_posts", type: "author_post", primary_key: false
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :name, :string
    end

    relationships do
      belongs_to :post, Ash.Test.Authorization.AuthorizationTest.Post, primary_key?: true
      belongs_to :author, Author, primary_key?: true
    end
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
    end

    relationships do
      has_many :author_posts, AuthorPost
      many_to_many :authors, Author, through: :author_posts
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, Author, AuthorPost]
  end

  test "it lives"
end
