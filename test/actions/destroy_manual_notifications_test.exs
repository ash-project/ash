# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Actions.DestroyManualNotificationsTest do
  @moduledoc false
  use ExUnit.Case, async: false

  alias Ash.Test.Domain, as: Domain

  defmodule Notifier do
    @moduledoc false
    use Ash.Notifier

    def notify(notification) do
      send(self(), {:notification, notification})
      :ok
    end
  end

  defmodule ManualDestroyReturningNotifications do
    @moduledoc false
    use Ash.Resource.ManualDestroy

    def destroy(changeset, _module_opts, _context) do
      extra =
        Ash.Notifier.Notification.new(changeset.resource,
          action: changeset.action,
          data: changeset.data,
          changeset: changeset,
          metadata: %{manual?: true}
        )

      {:ok, changeset.data, [extra]}
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [Notifier]

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:read, create: :*]

      destroy :manual_destroy do
        accept []
        require_atomic? false
        manual ManualDestroyReturningNotifications
      end
    end

    attributes do
      uuid_primary_key :id

      attribute :name, :string do
        public?(true)
      end
    end
  end

  defp create_post do
    Post
    |> Ash.Changeset.for_create(:create, %{name: "foo"})
    |> Ash.create!()
  end

  test "notifications returned as a bare list by a manual destroy are not dropped" do
    assert {:ok, _destroyed, notifications} =
             create_post()
             |> Ash.Changeset.for_destroy(:manual_destroy, %{})
             |> Ash.destroy(return_notifications?: true, return_destroyed?: true)

    assert Enum.any?(notifications, &(&1.metadata[:manual?] == true)),
           """
           expected the manual action's own notification to be returned, got:
           #{inspect(notifications)}
           """
  end

  test "the destroyed record's own notification is not dropped either" do
    post = create_post()
    # drop the notification the create above produced
    assert_received {:notification, %Ash.Notifier.Notification{action: %{name: :create}}}

    post
    |> Ash.Changeset.for_destroy(:manual_destroy, %{})
    |> Ash.destroy!()

    assert_received {:notification,
                     %Ash.Notifier.Notification{
                       resource: Post,
                       action: %{name: :manual_destroy}
                     }}
  end
end
