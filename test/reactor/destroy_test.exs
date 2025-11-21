# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.ReactorDestroyTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

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

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, allow_nil?: false, public?: true
      attribute :sub_title, :string, public?: true
      attribute :published, :boolean, default: false, public?: true
    end

    actions do
      default_accept :*
      defaults [:read, :destroy, create: :*, update: :*]

      create :undo_destroy do
        argument :record, :struct do
          allow_nil? false
          constraints instance_of: Post
        end

        change fn changeset, _ ->
          record = Ash.Changeset.get_argument(changeset, :record)

          [:id, :title, :sub_title, :published]
          |> Enum.reduce(changeset, fn key, changeset ->
            value = Map.get(record, key)
            Ash.Changeset.change_attribute(changeset, key, value)
          end)
        end
      end
    end

    code_interface do
      define :create
      define :get, get_by: :id, action: :read
    end
  end

  test "it can destroy a post" do
    defmodule SimpleDestroyPostReactor do
      @moduledoc false
      use Ash.Reactor

      input :post

      destroy :delete_post, Post, :destroy do
        initial input(:post)
      end
    end

    original_post = Post.create!(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, :ok} =
             Reactor.run(
               SimpleDestroyPostReactor,
               %{post: original_post},
               %{},
               async?: false
             )
  end

  test "it can destroy and return a post" do
    defmodule ReturningDestroyPostReactor do
      @moduledoc false
      use Ash.Reactor

      input :post

      destroy :delete_post, Post, :destroy do
        initial input(:post)
        return_destroyed? true
      end
    end

    original_post = Post.create!(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(
               ReturningDestroyPostReactor,
               %{post: original_post},
               %{},
               async?: false
             )

    assert original_post.__struct__ == post.__struct__
    assert original_post.id == post.id
    assert post.__meta__.state == :deleted
  end

  test "it can undo the destruction on error" do
    defmodule UndoingDestroyPostReactor do
      @moduledoc false
      use Ash.Reactor

      input :post

      destroy :delete_post, Post, :destroy do
        initial input(:post)

        undo :always
        undo_action :undo_destroy
      end

      step :fail do
        wait_for :delete_post

        run fn _, _ ->
          raise "hell"
        end
      end
    end

    post = Post.create!(%{title: "Title"})

    assert {:error, _} =
             Reactor.run(
               UndoingDestroyPostReactor,
               %{post: post},
               %{},
               async?: false
             )

    assert Post.get(post.id)
  end

  test "it can provide notification metadata" do
    defmodule DestroyWithNotificationMetadataReactor do
      @moduledoc false
      use Ash.Reactor

      input :post

      destroy :delete_post, Post, :destroy do
        initial input(:post)
        return_destroyed? true
        notification_metadata value(%{source: "reactor", operation: "destroy"})
      end
    end

    original_post = Post.create!(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(
               DestroyWithNotificationMetadataReactor,
               %{post: original_post},
               %{},
               async?: false
             )

    assert post.__meta__.state == :deleted

    # Check the destroy notification has metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :destroy},
                      metadata: %{source: "reactor", operation: "destroy"}
                    }}
  end

  test "it can provide notification metadata from a template" do
    defmodule DestroyWithTemplateNotificationMetadataReactor do
      @moduledoc false
      use Ash.Reactor

      input :post
      input :metadata

      destroy :delete_post, Post, :destroy do
        initial input(:post)
        return_destroyed? true
        notification_metadata input(:metadata)
      end
    end

    original_post = Post.create!(%{title: "Title", sub_title: "Sub-title"})
    metadata = %{reason: "cleanup", batch_id: "batch_999"}

    assert {:ok, post} =
             Reactor.run(
               DestroyWithTemplateNotificationMetadataReactor,
               %{post: original_post, metadata: metadata},
               %{},
               async?: false
             )

    assert post.__meta__.state == :deleted

    # Check the destroy notification has metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :destroy},
                      metadata: ^metadata
                    }}
  end

  test "it can transform notification metadata" do
    defmodule DestroyWithTransformedNotificationMetadataReactor do
      @moduledoc false
      use Ash.Reactor

      input :post
      input :metadata_input

      destroy :delete_post, Post, :destroy do
        initial input(:post)
        return_destroyed? true

        notification_metadata input(:metadata_input) do
          transform &Map.put(&1, :total_operations, &1.deleted_count + 10)
        end
      end
    end

    original_post = Post.create!(%{title: "Title", sub_title: "Sub-title"})

    assert {:ok, post} =
             Reactor.run(
               DestroyWithTransformedNotificationMetadataReactor,
               %{post: original_post, metadata_input: %{deleted_count: 1}},
               %{},
               async?: false
             )

    assert post.__meta__.state == :deleted

    # Check the destroy notification has transformed metadata
    assert_receive {:notification,
                    %Ash.Notifier.Notification{
                      action: %{type: :destroy},
                      metadata: %{deleted_count: 1, total_operations: 11}
                    }}
  end
end
