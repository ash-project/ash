# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.ReadParentExpressionTest do
  use ExUnit.Case, async: true

  alias Ash.Actions.Read.Relationships
  alias Ash.Test.Domain

  defmodule Author do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id
      attribute :created_at, :utc_datetime
    end

    relationships do
      has_many :posts_with_new_comments, Ash.Test.Actions.ReadParentExpressionTest.Post do
        destination_attribute :author_id
        filter expr(exists(comments, created_at > parent(updated_at)))
      end

      has_many :posts_after_signup, Ash.Test.Actions.ReadParentExpressionTest.Post do
        destination_attribute :author_id
        filter expr(created_at > parent(created_at))
      end
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id
      attribute :created_at, :utc_datetime
      attribute :updated_at, :utc_datetime
    end

    relationships do
      belongs_to :author, Author
      has_many :comments, Ash.Test.Actions.ReadParentExpressionTest.Comment
    end
  end

  defmodule Comment do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      defaults [:read]
    end

    attributes do
      uuid_primary_key :id
      attribute :created_at, :utc_datetime
    end

    relationships do
      belongs_to :post, Post
    end
  end

  test "a parent inside a post's comments filter does not reference the author" do
    relationship = Ash.Resource.Info.relationship(Author, :posts_with_new_comments)

    # Comment.created_at is compared with Post.updated_at, not an Author field.
    refute Relationships.has_parent_expr?(relationship, %{}, Domain)
  end

  test "a parent in the post filter references the author" do
    relationship = Ash.Resource.Info.relationship(Author, :posts_after_signup)

    # Post.created_at is compared with Author.created_at.
    assert Relationships.has_parent_expr?(relationship, %{}, Domain)
  end
end
