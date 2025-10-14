# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Reactor.NotificationsTest do
  @moduledoc false
  use ExUnit.Case, async: true
  use Mimic
  import ExUnit.CaptureLog
  alias Ash.Reactor.Notifications

  describe "init/1" do
    test "it starts an agent" do
      {:ok, context} = Notifications.init(%{})

      assert [] == agent_get(context.ash_notification_agent)
    end

    test "when there are already notifications in the context it stores them in the agent" do
      notifications = build_notifications()

      {:ok, context} =
        Notifications.init(%{ash_notifications: notifications})

      enqueued = agent_get(context.ash_notification_agent)
      assert enqueued == notifications
    end

    test "when there are already notifications in the context it removes them" do
      notifications = build_notifications()

      {:ok, context} =
        Notifications.init(%{ash_notifications: notifications})

      refute is_map_key(context, :ash_notifications)
    end
  end

  describe "halt/1" do
    setup do
      {:ok, context} = Notifications.init(%{})
      {:ok, context: context}
    end

    test "it stops the agent", %{context: context} do
      [agent | _] = context.ash_notification_agent

      {:ok, context} = Notifications.halt(context)
      assert context.ash_notification_agent == []
      refute Process.alive?(agent)
    end

    test "it moves any queued notifications into the context", %{context: context} do
      notifications = build_notifications()
      :ok = Notifications.enqueue_notifications(context, notifications)
      {:ok, context} = Notifications.halt(context)

      assert context.ash_notifications == notifications
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

      expect(Notifications, :publish, fn _context, actual ->
        assert actual == notifications
        []
      end)

      assert {:ok, :result} = Notifications.complete(:result, context)
    end

    test "it logs a warning when there are unpublished notifications", %{context: context} do
      notifications = build_notifications()
      :ok = Notifications.enqueue_notifications(context, notifications)

      expect(Notifications, :publish, fn _context, notifications -> notifications end)

      assert capture_log(fn ->
               assert {:ok, :result} = Notifications.complete(:result, context)
             end) =~ "Missed 3 notifications"
    end

    test "it stops the agent", %{context: context} do
      [agent | _] = context.ash_notification_agent

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
      [agent | _] = context.ash_notification_agent

      :ok = Notifications.error([:errors], context)
      refute Process.alive?(agent)
    end
  end

  defp build_notifications(how_many \\ 3) do
    for i <- 1..how_many do
      Ash.Notifier.Notification.new(__MODULE__.FakeResource, data: %{count: i})
    end
  end

  defp agent_get([agent | _]), do: Agent.get(agent, & &1)
end
