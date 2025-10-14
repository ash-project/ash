# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkUpdateTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.{Query, Test.Domain}

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer]

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :published_at, :datetime, allow_nil?: true, public?: true
    end

    actions do
      defaults create: :*

      update :publish do
        change set_attribute(:published_at, &DateTime.utc_now/0)
      end

      read :read do
        primary? true

        pagination keyset?: true, required?: false
      end
    end

    calculations do
      calculate :published?, :boolean, expr(published_at <= now())
    end

    policies do
      policy action(:publish) do
        forbid_if always()
      end

      policy always() do
        authorize_if always()
      end
    end

    ets do
      private? true
    end
  end

  defmodule BulkUpdateReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :posts_to_publish

    bulk_update :publish_posts, Post, :publish do
      initial(input(:posts_to_publish))
      return_errors?(true)
      authorize? false
    end
  end

  defmodule BulkUpdateForbiddenReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :posts_to_publish

    bulk_update :publish_posts, Post, :publish do
      initial(input(:posts_to_publish))
      return_errors?(true)
      authorize? true
    end
  end

  test "it can update a bunch of records all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    posts =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}", published_at: nil})
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    assert {:ok, _} =
             Reactor.run(BulkUpdateReactor, %{posts_to_publish: posts}, %{}, async?: false)

    updated_posts = Ash.read!(Post, action: :read, load: [:published?])

    assert Enum.all?(updated_posts, & &1.published?)
    assert length(updated_posts) == how_many
  end

  test "it can update a query all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    1..how_many
    |> Enum.map(&%{title: "Post number #{&1}", published_at: nil})
    |> Ash.bulk_create!(Post, :create)

    post_query =
      Post
      |> Query.for_read(:read)
      |> Query.filter(is_nil(published_at))

    assert {:ok, _} =
             Reactor.run(BulkUpdateReactor, %{posts_to_publish: post_query}, %{}, async?: false)

    updated_posts = Ash.read!(Post, action: :read, load: [:published?])

    assert Enum.all?(updated_posts, & &1.published?)
    assert length(updated_posts) == how_many
  end

  test "the bulk update can be forbidden by policies" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    posts =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}", published_at: nil})
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    assert {:error, %{errors: [%{error: %{errors: [error]}}]}} =
             Reactor.run(BulkUpdateForbiddenReactor, %{posts_to_publish: posts}, %{},
               async?: false
             )

    assert is_struct(error, Ash.Error.Forbidden)
  end
end
