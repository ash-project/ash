defmodule Ash.Test.Authorization.CreateAuthorizationTest do
  use ExUnit.Case, async: true

  defmodule Author do
    use Ash.Resource, name: "authors", type: "author"
    use Ash.DataLayer.Ets, private?: true

    actions do
      read :default

      create :default,
        authorization_steps: [
          authorize_if: user_attribute(:admin, true),
          authorize_if: user_attribute(:manager, true)
        ]
    end

    attributes do
      attribute :name, :string, authorization_steps: false

      attribute :state, :string,
        authorization_steps: [
          authorize_if: user_attribute(:admin, true),
          forbid_if: setting(to: "closed"),
          authorize_if: always()
        ]

      attribute :self_manager, :boolean, authorization_steps: false
      # [
      # authorize_if: user_attribute(:admin, true)
      # ]

      attribute :fired, :boolean, authorization_steps: false
      # [
      # authorize_if: user_attribute(:admin, true),
      # forbid_if: attribute_equals(:self_manager, true),
      # authorize_if: always()
      # ]
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
      attribute :manager, :boolean, default: {:constant, false}
      attribute :admin, :boolean, default: {:constant, false}
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

      create :default,
        authorization_steps: [
          authorize_if: user_attribute(:admin, true),
          authorize_if: user_attribute(:manager, true)
        ]
    end

    attributes do
      attribute :title, :string, authorization_steps: false

      attribute :contents, :string, authorization_steps: false

      attribute :published, :boolean, authorization_steps: false
    end

    relationships do
      has_many :author_posts, AuthorPost
      many_to_many :authors, Author, through: :author_posts
    end
  end

  defmodule Api do
    use Ash.Api

    resources [Post, Author, AuthorPost, User]
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
end
