# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkUpdateTest do
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
      initial input(:posts_to_publish)
      return_errors? true
      authorize? false
    end
  end

  defmodule BulkUpdateForbiddenReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :posts_to_publish

    bulk_update :publish_posts, Post, :publish do
      initial input(:posts_to_publish)
      return_errors? true
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
             Reactor.run(
               BulkUpdateReactor,
               %{posts_to_publish: posts},
               %{},
               async?: false
             )

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
             Reactor.run(
               BulkUpdateReactor,
               %{posts_to_publish: post_query},
               %{},
               async?: false
             )

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
             Reactor.run(
               BulkUpdateForbiddenReactor,
               %{posts_to_publish: posts},
               %{},
               async?: false
             )

    assert is_struct(error, Ash.Error.Forbidden)
  end

  test "it can provide notification metadata with a static value" do
    defmodule BulkUpdateWithNotificationMetadataReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :posts_to_publish

      bulk_update :publish_posts, Post, :publish do
        initial input(:posts_to_publish)
        notify? true
        return_records? true
        authorize? false
        notification_metadata %{source: "reactor", operation: "bulk_update"}
      end
    end

    posts =
      [%{title: "Post 1"}, %{title: "Post 2"}]
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    assert {:ok, %Ash.BulkResult{records: records}} =
             Reactor.run(
               BulkUpdateWithNotificationMetadataReactor,
               %{posts_to_publish: posts},
               %{},
               async?: false
             )

    assert length(records) == 2

    # Check that we received notifications with metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :update},
                      metadata: %{source: "reactor", operation: "bulk_update"}
                    }}

    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :update},
                      metadata: %{source: "reactor", operation: "bulk_update"}
                    }}
  end

  test "it can provide notification metadata from a template" do
    defmodule BulkUpdateWithTemplateNotificationMetadataReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      ash do
        default_domain(Domain)
      end

      input :posts_to_publish
      input :metadata

      bulk_update :publish_posts, Post, :publish do
        initial input(:posts_to_publish)
        notify? true
        return_records? true
        authorize? false
        notification_metadata input(:metadata)
      end
    end

    posts =
      [%{title: "Post 1"}, %{title: "Post 2"}]
      |> Ash.bulk_create!(Post, :create, return_records?: true)
      |> Map.fetch!(:records)

    metadata = %{batch_id: "batch_789", operation_id: "op_xyz"}

    assert {:ok, %Ash.BulkResult{records: records}} =
             Reactor.run(
               BulkUpdateWithTemplateNotificationMetadataReactor,
               %{posts_to_publish: posts, metadata: metadata},
               %{},
               async?: false
             )

    assert length(records) == 2

    # Check that we received notifications with the template metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :update},
                      metadata: ^metadata
                    }}

    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :update},
                      metadata: ^metadata
                    }}
  end
end
