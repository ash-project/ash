defmodule Ash.Notifier do
  @moduledoc """
  A notifier is an extension that receives various events
  """
  @callback notify(Ash.notification()) :: :ok

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Notifier
    end
  end

  @doc """
  Sends any notifications that can be sent, and returns the rest.

  A notification can only be sent if you are not currently in a transaction
  for the resource in question.
  """
  @spec notify(list(Ash.notification()) | Ash.notification()) :: list(Ash.notification())
  def notify(resource_notifications) do
    {unsent, to_send} =
      resource_notifications
      |> List.wrap()
      |> Enum.group_by(& &1.resource)
      |> Enum.split_with(fn {resource, _} ->
        resource && Ash.DataLayer.in_transaction?(resource)
      end)

    for {resource, notifications} <- to_send do
      for notification <- notifications do
        case notification.for do
          nil ->
            for notifier <- Ash.Resource.notifiers(resource) do
              notifier.notify(notification)
            end

          allowed_notifiers ->
            for notifier <- List.wrap(allowed_notifiers) do
              notifier.notify(notification)
            end
        end
      end
    end

    unsent
    |> Enum.map(&elem(&1, 1))
    |> List.flatten()
  end
end
