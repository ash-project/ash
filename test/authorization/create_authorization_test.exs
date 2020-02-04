defmodule Ash.Test.Authorization.CreateAuthorizationTest do
  use ExUnit.Case, async: true

  defmodule Draft do
    use Ash.Resource, name: "drafts", type: "draft"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default,
        rules: [
          authorize_if: always()
        ]

      create :default,
        rules: [
          forbid_unless: setting_relationship(:author),
          authorize_if: user_attribute(:author, true)
        ]
    end

    attributes do
      attribute :contents, :string, write_rules: false
      attribute :color, :string, write_rules: false
      attribute :size, :string, write_rules: false
    end

    relationships do
      belongs_to :author, Ash.Test.Authorization.CreateAuthorizationTest.Author,
        write_rules: [
          authorize_if: relating_to_user()
        ]
    end
  end

  defmodule Author do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default, rules: [authorize_if: always()]

      create :default,
        rules: [
          authorize_if: user_attribute(:admin, true),
          authorize_if: user_attribute(:manager, true)
        ]
    end

    attributes do
      attribute :name, :string, write_rules: false

      attribute :state, :string,
        write_rules: [
          authorize_if: user_attribute(:admin, true),
          forbid_if: setting(to: "closed"),
          authorize_if: always()
        ]

      attribute :bio_locked, :boolean,
        default: {:constant, false},
        write_rules: false

      attribute :self_manager, :boolean, write_rules: false

      attribute :fired, :boolean, write_rules: false
    end

    relationships do
      many_to_many :posts, Ash.Test.Authorization.CreateAuthorizationTest.Post,
        through: Ash.Test.Authorization.CreateAuthorizationTest.AuthorPost

      has_one :bio, Ash.Test.Authorization.CreateAuthorizationTest.Bio,
        write_rules: [
          forbid_if: attribute_equals(:bio_locked, true),
          authorize_if: always()
        ]
    end
  end

  defmodule Bio do
    use Ash.Resource, name: "bios", type: "bio"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default,
        rules: [
          authorize_if: always()
        ]

      create :default,
        rules: [
          forbid_unless: setting_relationship(:author),
          authorize_if: user_attribute(:author, true)
        ]

      update :default,
        rules: [
          authorize_if: always()
        ]
    end

    attributes do
      attribute :admin_only?, :boolean,
        default: {:constant, false},
        write_rules: [
          authorize_if: always()
        ]
    end

    relationships do
      belongs_to :author, Author,
        write_rules: [
          authorize_if: relating_to_user()
        ]
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
      attribute :manager, :boolean, default: {:constant, false}
      attribute :admin, :boolean, default: {:constant, false}
      attribute :author, :boolean, default: {:constant, false}
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
      belongs_to :post, Ash.Test.Authorization.CreateAuthorizationTest.Post, primary_key?: true
      belongs_to :author, Author, primary_key?: true
    end
  end

  defmodule Post do
    use Ash.Resource, name: "posts", type: "post"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default

      create :default,
        rules: [
          authorize_if: user_attribute(:admin, true),
          authorize_if: user_attribute(:manager, true)
        ]
    end

    attributes do
      attribute :title, :string, write_rules: false

      attribute :contents, :string, write_rules: false

      attribute :published, :boolean, write_rules: false
    end

    relationships do
      many_to_many :authors, Author, through: AuthorPost
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, Author, AuthorPost, User, Draft, Bio]
  end

  test "should fail if a user does not match the action requirements" do
    user = Api.create!(User, attributes: %{name: "foo", admin: false, manager: false})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.create!(Author, attributes: %{name: "foo"}, authorization: [user: user])
    end
  end

  test "should fail if a change is not authorized" do
    user = Api.create!(User, attributes: %{name: "foo", manager: true})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.create!(Author,
        attributes: %{name: "foo", state: "closed"},
        authorization: [user: user]
      )
    end
  end

  test "should succeed if a change is authorized" do
    user = Api.create!(User, attributes: %{name: "foo", manager: true})

    Api.create!(Author, attributes: %{name: "foo", state: "open"}, authorization: [user: user])
  end

  test "forbids belongs_to relationships properly" do
    user = Api.create!(User, attributes: %{name: "foo", author: true})
    author = Api.create!(Author, attributes: %{name: "someone else"})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.create!(Draft,
        attributes: %{contents: "best ever"},
        relationships: %{author: author.id},
        authorization: [user: user]
      )
    end
  end

  test "allows belongs_to relationships properly" do
    user = Api.create!(User, attributes: %{name: "foo", author: true})
    author = Api.create!(Author, attributes: %{name: "someone else", id: user.id})

    Api.create!(Draft,
      attributes: %{contents: "best ever"},
      relationships: %{author: author.id},
      authorization: [user: user]
    )
  end

  test "it forbids has_one relationships properly" do
    user = Api.create!(User, attributes: %{name: "foo", author: true, manager: true})

    bio = Api.create!(Bio, attributes: %{admin_only?: false})

    assert_raise Ash.Error.Forbidden, ~r/forbidden/, fn ->
      Api.create!(Author,
        attributes: %{contents: "best ever", bio_locked: true},
        relationships: %{bio: bio.id},
        authorization: [user: user]
      )
    end
  end

  test "it allows has_one relationships properly" do
    user = Api.create!(User, attributes: %{name: "foo", author: true, manager: true})

    bio = Api.create!(Bio, attributes: %{admin_only?: false})

    Api.create!(Author,
      attributes: %{contents: "best ever"},
      relationships: %{bio: bio.id},
      authorization: [user: user]
    )
  end
end
