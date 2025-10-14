# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

alias Ash.Test.AggregationCheckTest, as: ThisTest
alias Ash.Changeset

defmodule ThisTest.Domain do
  use Ash.Domain

  resources do
    allow_unregistered? true
  end
end

defmodule ThisTest.Post do
  use Ash.Resource,
    authorizers: [Ash.Policy.Authorizer],
    data_layer: Ash.DataLayer.Ets,
    domain: ThisTest.Domain

  actions do
    defaults [:read, create: :*, update: :*]

    update :append_comment do
      require_atomic? false
      argument :comment, :map, allow_nil?: false
      change manage_relationship(:comment, :comments, type: :create)
    end

    update :update_if_no_comment do
      accept :*
    end

    update :update_if_no_comment_inline do
      accept :*
    end

    update :update_unless_invalid_comment do
      accept :*
    end

    update :update_unless_invalid_comment_inline do
      accept :*
    end
  end

  policies do
    policy action(:update_if_no_comment) do
      forbid_if expr(comment_count > 0)
      authorize_if always()
    end

    policy action(:update_if_no_comment_inline) do
      forbid_if expr(count(comments) > 0)
      authorize_if always()
    end

    policy action(:update_unless_invalid_comment) do
      forbid_if expr(has_invalid_comment)
      authorize_if always()
    end

    policy action(:update_unless_invalid_comment_inline) do
      forbid_if expr(exists(comments, content == "invalid"))
      authorize_if always()
    end

    policy always() do
      authorize_if always()
    end
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :content, :string, allow_nil?: false, public?: true
  end

  relationships do
    has_many :comments, ThisTest.Comment
  end

  aggregates do
    count :comment_count, :comments
    exists :has_invalid_comment, :comments, filter: expr(content == "invalid")
  end
end

defmodule ThisTest.Comment do
  use Ash.Resource,
    data_layer: Ash.DataLayer.Ets,
    domain: ThisTest.Domain

  actions do
    defaults [:read, create: :*]
  end

  attributes do
    uuid_v7_primary_key :id
    attribute :content, :string, allow_nil?: false, public?: true
  end

  relationships do
    belongs_to :post, ThisTest.Post
  end
end

defmodule ThisTest do
  use ExUnit.Case, async: true

  setup do
    post = Changeset.for_create(ThisTest.Post, :create, %{content: "post"}) |> Ash.create!()
    %{post: post}
  end

  test "policy can check count aggregation", %{post: post} do
    assert {:ok, true} = Ash.can({post, :update_if_no_comment}, nil)

    params = %{comment: %{content: "comment"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)
    assert {:ok, false} = Ash.can({post, :update_if_no_comment}, nil)
  end

  test "policy can check inline count aggregation", %{post: post} do
    assert {:ok, true} = Ash.can({post, :update_if_no_comment_inline}, nil)

    params = %{comment: %{content: "comment"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)
    assert {:ok, false} = Ash.can({post, :update_if_no_comment_inline}, nil)
  end

  test "policy can check exist aggregation", %{post: post} do
    params = %{comment: %{content: "valid"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)
    assert {:ok, true} = Ash.can({post, :update_unless_invalid_comment}, nil)

    params = %{comment: %{content: "invalid"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)
    assert {:ok, false} = Ash.can({post, :update_unless_invalid_comment}, nil)
  end

  test "policy can check inline exist aggregation", %{post: post} do
    params = %{comment: %{content: "valid"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)
    assert {:ok, true} = Ash.can({post, :update_unless_invalid_comment_inline}, nil)

    params = %{comment: %{content: "invalid"}}
    post = Changeset.for_update(post, :append_comment, params) |> Ash.update!()

    post = Ash.get!(ThisTest.Post, post.id)

    assert {:ok, false} = Ash.can({post, :update_unless_invalid_comment_inline}, nil)
  end
end
