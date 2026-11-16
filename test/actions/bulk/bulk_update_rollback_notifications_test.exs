# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.BulkUpdateRollbackNotificationsTest do
  @moduledoc false
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
      :ok
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Actions.BulkUpdateRollbackNotificationsTest.Domain,
      data_layer: Ash.DataLayer.Mnesia,
      notifiers: [Notifier]

    attributes do
      uuid_primary_key :id
      attribute :body, :string, public?: true
    end

    relationships do
      belongs_to :post, Ash.Test.Actions.BulkUpdateRollbackNotificationsTest.Post, public?: true
    end

    actions do
      defaults [:read, create: [:body, :post_id]]
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Ash.Test.Actions.BulkUpdateRollbackNotificationsTest.Domain,
      data_layer: Ash.DataLayer.Mnesia

    attributes do
      uuid_primary_key :id
      attribute :fail, :boolean, default: false, public?: true
    end

    relationships do
      has_many :comments, Comment, public?: true
    end

    actions do
      defaults [:read, create: [:fail]]

      update :add_comments do
        require_atomic? false
        touches_resources [Comment]
        argument :comments, {:array, :map}, allow_nil?: false
        change manage_relationship(:comments, :comments, type: :create)

        change after_action(fn _changeset, record, _context ->
                 if record.fail, do: {:error, "boom"}, else: {:ok, record}
               end)
      end
    end
  end

  defmodule Domain do
    @moduledoc false
    use Ash.Domain, validate_config_inclusion?: false

    resources do
      resource Post do
        define :add_comments, args: [:comments]
      end

      resource Comment
    end
  end

  setup do
    capture_log(fn -> Ash.DataLayer.Mnesia.start(Domain, [Post, Comment]) end)

    on_exit(fn ->
      capture_log(fn ->
        :mnesia.stop()
        :mnesia.delete_schema([node()])
      end)
    end)

    :ok
  end

  test "single-record code interface updates discard rolled-back relationship notifications" do
    post = Ash.create!(Post, %{fail: true})

    assert {:error, _} = Domain.add_comments(post, [%{body: "rolled back"}])
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "code interface updates by id discard rolled-back relationship notifications" do
    post = Ash.create!(Post, %{fail: true})

    assert {:error, _} = Domain.add_comments(post.id, [%{body: "rolled back"}])
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "bulk updates do not send notifications from a rolled-back batch" do
    post = Ash.create!(Post, %{fail: true})

    result = bulk_update([post], notify?: true)

    assert result.status == :error
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "bulk updates do not return notifications from a rolled-back batch" do
    post = Ash.create!(Post, %{fail: true})

    result = bulk_update([post], return_notifications?: true)

    assert result.status == :error
    assert result.notifications == []
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "a failed batch preserves returned notifications from earlier committed batches" do
    successful = Ash.create!(Post, %{})
    failing = Ash.create!(Post, %{fail: true})

    result = bulk_update([successful, failing], batch_size: 1, return_notifications?: true)

    assert result.error_count == 1
    assert [%{post_id: post_id, id: comment_id}] = Ash.read!(Comment)
    assert post_id == successful.id

    assert [%{resource: Comment, data: %{id: ^comment_id}}] =
             Enum.filter(result.notifications, &(&1.resource == Comment))

    refute_received {:notification, _}
  end

  test "a later failure discards relationship notifications for the entire batch" do
    successful = Ash.create!(Post, %{})
    failing = Ash.create!(Post, %{fail: true})

    result = bulk_update([successful, failing], batch_size: 2, notify?: true)

    assert result.error_count == 2
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "streamed results do not send notifications from a rolled-back batch" do
    post = Ash.create!(Post, %{fail: true})

    results =
      [post]
      |> bulk_update(return_stream?: true, notify?: true)
      |> Enum.to_list()

    assert [{:error, _}] = results
    assert [] = Ash.read!(Comment)
    refute_received {:notification, _}
  end

  test "successful batches send notifications for committed relationships" do
    post = Ash.create!(Post, %{})

    result = bulk_update([post], notify?: true)

    assert result.status == :success
    assert [%{id: comment_id}] = Ash.read!(Comment)
    assert_received {:notification, %{resource: Comment, data: %{id: ^comment_id}}}
  end

  test "notifications are preserved when rollback is disabled and relationships persist" do
    post = Ash.create!(Post, %{fail: true})

    result = bulk_update([post], rollback_on_error?: false, notify?: true)

    assert result.status == :error
    assert [%{id: comment_id}] = Ash.read!(Comment)
    assert_received {:notification, %{resource: Comment, data: %{id: ^comment_id}}}
  end

  defp bulk_update(posts, opts) do
    Ash.bulk_update(
      posts,
      :add_comments,
      %{comments: [%{body: "created by relationship"}]},
      Keyword.merge(
        [
          resource: Post,
          strategy: [:stream],
          transaction: :batch,
          stop_on_error?: false,
          return_errors?: true,
          authorize?: false
        ],
        opts
      )
    )
  end
end
