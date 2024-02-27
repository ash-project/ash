defmodule Ash.Test.Reactor.NotificationsTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use Mimic
  import ExUnit.CaptureLog
  alias Ash.Reactor.Notifications

  describe "init/1" do
    test "it starts an agent" do
      {:ok, context} = Notifications.init(%{})

      assert [] == agent_get(context.__ash_notification_agent__)
    end

    test "when there are already notifications in the context it stores them in the agent" do
      notifications = build_notifications()

      {:ok, context} =
        Notifications.init(%{__unpublished_ash_notifications__: notifications})

      enqueued = agent_get(context.__ash_notification_agent__)
      assert enqueued == notifications
    end

    test "when there are already notifications in the context it removes them" do
      notifications = build_notifications()

      {:ok, context} =
        Notifications.init(%{__unpublished_ash_notifications__: notifications})

      refute is_map_key(context, :__unpublished_ash_notifications__)
    end
  end

  describe "halt/1" do
    setup do
      {:ok, context} = Notifications.init(%{})
      {:ok, context: context}
    end

    test "it stops the agent", %{context: context} do
      agent = context.__ash_notification_agent__

      {:ok, context} = Notifications.halt(context)
      refute is_map_key(context, :__ash_notification_agent__)
      refute Process.alive?(agent)
    end

    test "it moves any queued notifications into the context", %{context: context} do
      notifications = build_notifications()
      :ok = Notifications.enqueue_notifications(context, notifications)
      {:ok, context} = Notifications.halt(context)

      assert context.__unpublished_ash_notifications__ == notifications
    end
  end

  describe "complete/2" do
    setup do
      {:ok, context} = Notifications.init(%{})
      {:ok, context: context}
    end

    test "it publishes any queued notifications", %{context: context} do
      notifications = build_notifications()
      :ok = Notifications.enqueue_notifications(context, notifications)

      expect(Ash.Notifier, :notify, fn actual ->
        assert actual == notifications
        []
      end)

      assert {:ok, :result} = Notifications.complete(:result, context)
    end

    test "it logs a warning when there are unpublished notifications", %{context: context} do
      notifications = build_notifications()
      :ok = Notifications.enqueue_notifications(context, notifications)

      expect(Ash.Notifier, :notify, & &1)

      assert capture_log(fn ->
               assert {:ok, :result} = Notifications.complete(:result, context)
             end) =~ "Missed 3 notifications"
    end

    test "it stops the agent", %{context: context} do
      agent = context.__ash_notification_agent__

      {:ok, :result} = Notifications.complete(:result, context)
      refute Process.alive?(agent)
    end
  end

  describe "error/2" do
    setup do
      {:ok, context} = Notifications.init(%{})
      {:ok, context: context}
    end

    test "it stops the agent", %{context: context} do
      agent = context.__ash_notification_agent__

      :ok = Notifications.error([:errors], context)
      refute Process.alive?(agent)
    end
  end

  defp build_notifications(how_many \\ 3) do
    for i <- 1..how_many do
      Ash.Notifier.Notification.new(__MODULE__.FakeResource, data: %{count: i})
    end
  end

  defp agent_get(agent), do: Agent.get(agent, & &1)
end
