# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Policy.CanReadUnrelatedExistsTest do
  @doc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule User do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :roles, Ash.Test.Policy.CanReadUnrelatedExistsTest.Role, public?: true
    end
  end

  defmodule Role do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    relationships do
      belongs_to :user, User, public?: true
    end
  end

  defmodule Post do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    policies do
      # Depends only on the actor: an `exists` over an unrelated resource
      # that follows one of that resource's relationships.
      policy action_type(:read) do
        authorize_if expr(exists(User, id == ^actor(:id) and exists(roles, name == "editor")))
      end
    end
  end

  defmodule Comment do
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    ets do
      private? true
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      belongs_to :post, Post, public?: true
    end

    policies do
      policy action_type(:read) do
        authorize_if can_read(:post)
      end
    end
  end

  test "can_read keeps an unrelated exists scoped to its own resource" do
    editor = Ash.create!(User, %{})
    Ash.create!(Role, %{name: "editor", user_id: editor.id})
    reader = Ash.create!(User, %{})

    post = Ash.create!(Post, %{}, authorize?: false)
    comment = Ash.create!(Comment, %{post_id: post.id}, authorize?: false)

    # The post's own policy works.
    assert [%{id: post_id}] = Ash.read!(Post, actor: editor)
    assert post_id == post.id
    assert [] = Ash.read!(Post, actor: reader)

    # Through can_read, `roles` is looked up on Comment instead of User.
    assert [%{id: comment_id}] = Ash.read!(Comment, actor: editor)
    assert comment_id == comment.id
    assert [] = Ash.read!(Comment, actor: reader)
  end
end
