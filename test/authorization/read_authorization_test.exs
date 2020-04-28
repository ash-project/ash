defmodule Ash.Test.Authorization.ReadAuthorizationTest do
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
      attribute :manager, :boolean, default: {:constant, false}, allow_nil?: false
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

  test "it succeeds if you match the first rule" do
    author = Api.create!(Author, attributes: %{name: "foo"})
    user = Api.create!(User, attributes: %{id: author.id})

    Api.read!(Post,
      authorization: [user: user],
      filter: [authors: [id: author.id]]
    )
  end

  test "it succeeds if you match the second rule" do
    user = Api.create!(User)

    Api.read!(Post,
      authorization: [user: user],
      filter: [published: true]
    )
  end

  test "it succeeds if you match both rules" do
    author = Api.create!(Author, attributes: %{name: "foo"})
    user = Api.create!(User, attributes: %{id: author.id})

    Api.read!(Post,
      authorization: [user: user],
      filter: [published: true, authors: [id: author.id]]
    )
  end

  test "it fails if you don't match either" do
    user = Api.create!(User)

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Post,
        authorization: [user: user],
        filter: [published: false]
      )
    end
  end

  test "it fails if it can't confirm that you match either" do
    user = Api.create!(User)

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Post, authorization: [user: user])
    end
  end

  test "authorize_if falls through properly" do
    user = Api.create!(User, attributes: %{manager: true})

    Api.read!(Author,
      filter: [fired: [not_eq: true], self_manager: [not_eq: true]],
      authorization: [user: user]
    )
  end

  test "authorize_unless doesn't trigger if its check is not true" do
    user = Api.create!(User, attributes: %{manager: true})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Author,
        filter: [fired: false, self_manager: true],
        authorization: [user: user]
      )
    end
  end

  test "forbid_if triggers if its check is true" do
    user = Api.create!(User, attributes: %{manager: true})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Author,
        filter: [fired: true, self_manager: false],
        authorization: [user: user]
      )
    end
  end

  test "forbid_unless doesn't trigger if its check is true" do
    user = Api.create!(User, attributes: %{manager: false})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Author,
        filter: [fired: false, self_manager: false],
        authorization: [user: user]
      )
    end
  end

  test "it can handle conflicting results" do
    author = Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: true})

    Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: false})

    user = Api.create!(User, attributes: %{manager: true, id: author.id})

    Api.read!(Author, authorization: [user: user], bypass_strict_access?: true)
  end

  test "it fails properly on conflicting results" do
    author = Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: true})
    Api.create!(Author, attributes: %{name: "foo", fired: false, self_manager: false})
    user = Api.create!(User, attributes: %{manager: false, id: author.id})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.read!(Author,
        authorization: [user: user, bypass_strict_access?: true]
      )
    end
  end

  test "it handles authorizing destination records properly" do
    author = Api.create!(Author, attributes: %{name: "foo"})
    user = Api.create!(User, attributes: %{manager: true})

    Api.read!(Post,
      authorization: [user: user],
      filter: [published: true, authors: [id: author.id]]
    )
  end
end
