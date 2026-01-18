# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: false

  require Ash.Query
  alias Ash.{Query, Test.Domain}

  defmodule TestNotifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
      :ok
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      data_layer: Ash.DataLayer.Ets,
      domain: Domain,
      authorizers: [Ash.Policy.Authorizer],
      notifiers: [TestNotifier]

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :published_at, :datetime, allow_nil?: true, public?: true
    end

    actions do
      defaults [:read, :destroy, create: :*]

      read :published do
        filter expr(not is_nil(published_at))
        pagination keyset?: true, required?: false
      end

      destroy :archive do
        soft? true
        change set_attribute(:published_at, nil)
      end
    end

    policies do
      policy action(:archive) do
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

  defmodule BulkDestroyReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :posts_to_destroy

    bulk_destroy :destroy_posts, Post, :destroy do
      initial input(:posts_to_destroy)
      return_errors? true
      authorize? false
    end
  end

  defmodule BulkDestroyForbiddenReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :posts_to_archive

    bulk_destroy :archive_posts, Post, :archive do
      initial input(:posts_to_archive)
      return_errors? true
      authorize? true
    end
  end

  test "it can destroy a bunch of records all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    posts =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}"})
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    assert {:ok, _} =
             Reactor.run(
               BulkDestroyReactor,
               %{posts_to_destroy: posts},
               %{},
               async?: false
             )

    remaining_posts = Ash.read!(Post)
    assert Enum.empty?(remaining_posts)
  end

  test "it can destroy a query all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    1..how_many
    |> Enum.map(&%{title: "Post number #{&1}", published_at: DateTime.utc_now()})
    |> Ash.bulk_create!(Post, :create)

    post_query =
      Post
      |> Query.for_read(:published)
      |> Query.filter(not is_nil(published_at))

    assert {:ok, _} =
             Reactor.run(
               BulkDestroyReactor,
               %{posts_to_destroy: post_query},
               %{},
               async?: false
             )

    remaining_posts = Ash.read!(Post)
    assert Enum.empty?(remaining_posts)
  end

  test "the bulk destroy can be forbidden by policies" do
    how_many = :rand.uniform(99) + :rand.uniform(99)

    posts =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}"})
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    assert {:error, %{errors: [%{error: %{errors: [error]}}]}} =
             Reactor.run(
               BulkDestroyForbiddenReactor,
               %{posts_to_archive: posts},
               %{},
               async?: false
             )

    assert is_struct(error, Ash.Error.Forbidden)
  end

  # Note: notification_metadata tests are skipped because the underlying
  # Ash.Actions.Destroy.Bulk module does not yet propagate notification_metadata
  # in atomic destroy mode. The reactor step correctly passes the option,
  # but the core library doesn't use it yet.
end
