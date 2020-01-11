defmodule Ash.Test.Authorization.GetAuthorizationTest do
  use ExUnit.Case, async: true

  defmodule Author do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default,
        rules: [
          # You can see yourself
          authorize_if: user_attribute_matches_record(:id, :id),
          # You can't see anything else unless you're a manager
          forbid_unless: user_attribute(:manager, true),
          # No one can see a fired author
          forbid_if: attribute_equals(:fired, true),
          # Managers can't see `self_manager` authors
          authorize_unless: attribute_equals(:self_manager, true)
        ]

      create :default
    end

    attributes do
      attribute :name, :string
      attribute :self_manager, :boolean
      attribute :fired, :boolean
    end

    relationships do
      many_to_many :posts, Ash.Test.Authorization.AuthorizationTest.Post,
        through: Ash.Test.Authorization.AuthorizationTest.AuthorPost

      has_many :drafts, Draft
    end
  end

  defmodule User do
    use Ash.Resource, name: "users", type: "user"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default
      create :default
    end

    attributes do
      attribute :name, :string
      attribute :manager, :boolean
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
      read :default,
        rules: [
          authorize_if: attribute_equals(:published, true),
          authorize_if: related_to_user_via(:authors)
        ]

      create :default
    end

    attributes do
      attribute :title, :string
      attribute :contents, :string
      attribute :published, :boolean
    end

    relationships do
      has_many :author_posts, AuthorPost
      many_to_many :authors, Author, through: AuthorPost
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, Author, AuthorPost, User]
  end

  test "it succeeds if you match a strict_check" do
    author = Api.create!(Author, attributes: %{name: "foo"})
    user = Api.create!(User, attributes: %{id: author.id})

    Api.get!(Author, author.id, authorization: [user: user])
  end

  test "it succeeds if you match the data checks" do
    author = Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: false})
    user = Api.create!(User, attributes: %{manager: true})

    Api.get!(Author, author.id, authorization: [user: user])
  end

  test "it fails if you dont match the data checks" do
    author = Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: true})

    user = Api.create!(User, attributes: %{manager: true})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.get!(Author, author.id, authorization: [user: user])
    end
  end
end
