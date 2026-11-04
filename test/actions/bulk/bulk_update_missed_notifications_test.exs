# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.Bulk.BulkUpdateMissedNotificationsTest do
  @moduledoc false
  # Not async because of Mnesia.
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  require Ash.Query

  alias Ash.Test.Domain

  defmodule Post do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :status, :string, public?: true
    end

    actions do
      defaults [:read, create: [:status]]

      update :archive do
        change set_attribute(:status, "archived")
      end
    end
  end

  defmodule Comment do
    @moduledoc false
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Mnesia

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]
    end
  end

  setup do
    Ash.DataLayer.Mnesia.start(Domain, [Comment])
    :ok
  end

  # Ets has no transactions and Mnesia has no `update_query`, so no data layer
  # here runs an atomic bulk update inside a transaction. A queued notification
  # of a Mnesia resource gets the same result: it cannot be sent inside the
  # Mnesia transaction, so the atomic update reports it as missed.
  test "an atomic bulk update given the action's name can report missed notifications" do
    post = Post |> Ash.Changeset.for_create(:create, %{status: "active"}) |> Ash.create!()

    missed = %Ash.Notifier.Notification{resource: Comment, domain: Domain, for: []}

    log =
      capture_log(fn ->
        assert {:atomic, %Ash.BulkResult{status: :success}} =
                 :mnesia.transaction(fn ->
                   Ash.Actions.Helpers.queue_notifications([missed])

                   Post
                   |> Ash.Query.filter(id == ^post.id)
                   |> Ash.bulk_update(:archive, %{}, strategy: :atomic, notify?: true)
                 end)
      end)

    assert log =~ "Missed 1 notifications in action"
    assert log =~ "BulkUpdateMissedNotificationsTest.Post.archive"
  end
end
