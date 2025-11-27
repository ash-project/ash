# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.BulkCreateTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

  require Ash.Query

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
      notifiers: [TestNotifier]

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :nonce, :integer, allow_nil?: true, public?: true
    end

    actions do
      defaults [:read, create: :*]
    end
  end

  defmodule BulkCreateReactor do
    @moduledoc false
    use Reactor, extensions: [Ash.Reactor]

    input :post_attrs

    bulk_create :create_posts, Post, :create do
      initial input(:post_attrs)
    end
  end

  test "it can create a bunch of records all at once" do
    how_many = :rand.uniform(99) + :rand.uniform(99)
    # Isolate this test from other async tests with a unique nonce value
    nonce = :rand.uniform(1_000_000)

    post_attrs =
      1..how_many
      |> Enum.map(&%{title: "Post number #{&1}", nonce: nonce})

    assert {:ok, _} =
             Reactor.run(
               BulkCreateReactor,
               %{post_attrs: post_attrs},
               %{},
               async?: false
             )

    created_posts =
      Post
      |> Ash.Query.filter(nonce: nonce)
      |> Ash.read!(action: :read)

    assert length(created_posts) == how_many
  end

  test "it can provide notification metadata with a static value" do
    defmodule BulkCreateWithNotificationMetadataReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :post_attrs

      bulk_create :create_posts, Post, :create do
        initial input(:post_attrs)
        notify? true
        return_records? true
        notification_metadata %{source: "reactor", operation: "bulk_create"}
      end
    end

    post_attrs = [%{title: "Post 1"}, %{title: "Post 2"}]

    assert {:ok, %Ash.BulkResult{records: records}} =
             Reactor.run(
               BulkCreateWithNotificationMetadataReactor,
               %{post_attrs: post_attrs},
               %{},
               async?: false
             )

    assert length(records) == 2

    # Check that we received notifications with metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :create},
                      metadata: %{source: "reactor", operation: "bulk_create"}
                    }}

    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :create},
                      metadata: %{source: "reactor", operation: "bulk_create"}
                    }}
  end

  test "it can provide notification metadata from a template" do
    defmodule BulkCreateWithTemplateNotificationMetadataReactor do
      @moduledoc false
      use Reactor, extensions: [Ash.Reactor]

      input :post_attrs
      input :metadata

      bulk_create :create_posts, Post, :create do
        initial input(:post_attrs)
        notify? true
        return_records? true
        notification_metadata input(:metadata)
      end
    end

    post_attrs = [%{title: "Post 1"}, %{title: "Post 2"}]
    metadata = %{batch_id: "batch_123", user_id: "user_456"}

    assert {:ok, %Ash.BulkResult{records: records}} =
             Reactor.run(
               BulkCreateWithTemplateNotificationMetadataReactor,
               %{post_attrs: post_attrs, metadata: metadata},
               %{},
               async?: false
             )

    assert length(records) == 2

    # Check that we received notifications with the template metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :create},
                      metadata: ^metadata
                    }}

    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :create},
                      metadata: ^metadata
                    }}
  end
end
