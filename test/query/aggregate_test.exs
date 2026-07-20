# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.AggregateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain

  defmodule Comment do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      create :create do
        primary? true
        accept [:post_id, :priority]
      end

      read :comments_default_read do
        primary? true
      end

      read :comments_read do
        argument :min_priority, :integer, allow_nil?: false, default: 10
        filter expr(priority >= ^arg(:min_priority))
      end

      read :alternate_comments_read
      read :explicit_comments_read
    end

    attributes do
      uuid_primary_key :id
      attribute :post_id, :uuid
      attribute :priority, :integer
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      create :create do
        primary? true
        accept [:author_id]
      end

      read :posts_default_read do
        primary? true
      end

      read :posts_read
      read :alternate_posts_read
    end

    attributes do
      uuid_primary_key :id
      attribute :author_id, :uuid
    end

    relationships do
      has_many :comments, Comment do
        destination_attribute :post_id
        read_action :comments_read
        read_action_arguments %{min_priority: 50}
      end

      has_many :alternate_comments, Comment do
        destination_attribute :post_id
        read_action :alternate_comments_read
      end

      has_many :default_comments, Comment do
        destination_attribute :post_id
      end
    end

    aggregates do
      count :comment_count, :comments
    end
  end

  defmodule Author do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    actions do
      create :create do
        primary? true
        accept []
      end

      read :authors_default_read do
        primary? true
      end
    end

    attributes do
      uuid_primary_key :id
    end

    relationships do
      has_many :posts, Post do
        destination_attribute :author_id
        read_action :posts_read
      end

      has_many :alternate_posts, Post do
        destination_attribute :author_id
        read_action :alternate_posts_read
      end

      has_many :default_posts, Post do
        destination_attribute :author_id
      end
    end

    aggregates do
      count :post_count, :posts
      count :default_post_count, :default_posts
      count :comment_count, [:posts, :comments]
      count :comment_count_from_default_posts, [:default_posts, :comments]
      count :alternate_comment_count, [:alternate_posts, :alternate_comments]
      count :default_comment_count, [:posts, :default_comments]

      count :explicit_comment_count, [:posts, :comments] do
        read_action :explicit_comments_read
      end
    end
  end

  test "aggregates use explicit actions or the action of the final relationship" do
    actions =
      Author
      |> Ash.Query.load([
        :post_count,
        :default_post_count,
        :comment_count,
        :comment_count_from_default_posts,
        :alternate_comment_count,
        :default_comment_count,
        :explicit_comment_count
      ])
      |> Map.fetch!(:aggregates)
      |> Map.new(fn {name, aggregate} ->
        {name, {aggregate.read_action, aggregate.query.action && aggregate.query.action.name}}
      end)

    assert actions == %{
             post_count: {:posts_read, :posts_read},
             default_post_count: {nil, nil},
             comment_count: {:comments_read, :comments_read},
             comment_count_from_default_posts: {:comments_read, :comments_read},
             alternate_comment_count: {:alternate_comments_read, :alternate_comments_read},
             default_comment_count: {nil, nil},
             explicit_comment_count: {:explicit_comments_read, :explicit_comments_read}
           }
  end

  test "single-hop and multi-hop aggregates pass the final relationship's read action arguments" do
    author = Author |> Ash.Changeset.for_create(:create) |> Ash.create!()

    post =
      Post
      |> Ash.Changeset.for_create(:create, %{author_id: author.id})
      |> Ash.create!()

    for priority <- [20, 60] do
      Comment
      |> Ash.Changeset.for_create(:create, %{post_id: post.id, priority: priority})
      |> Ash.create!()
    end

    assert %{comment_count: 1} = Ash.load!(post, :comment_count)

    assert %{comment_count: 1, explicit_comment_count: 2} =
             Ash.load!(author, [:comment_count, :explicit_comment_count])
  end
end
