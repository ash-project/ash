defmodule Ash.Reactor.Notifications do
  @moduledoc """
  Reactor middleware used to collect and emit notifications upon successful
  completion of the Reactor.
  """

  use Reactor.Middleware

  require Logger

  @agent_key :ash_notification_agent
  @notification_key :ash_notifications

  defguardp has_agent?(context)
            when is_map_key(context, @agent_key) and
                   :erlang.map_get(@agent_key, context) != []

  defguardp has_notifications?(context)
            when is_map_key(context, @notification_key) and
                   :erlang.map_get(@notification_key, context) != []

  @doc """
  When starting a reactor, start an agent to act as a temporary store of
  notifications.
  """
  @impl true
  def init(context) when has_notifications?(context) do
    with {:ok, notifications} <- Map.fetch(context, @notification_key),
         {:ok, context} <- agent_start(context),
         {:ok, context} <- agent_put(context, notifications) do
      context =
        context
        |> Map.delete(@notification_key)

      {:ok, context}
    end
  end

  def init(context), do: agent_start(context)

  @doc """
  When halting the reactor, store any queued notifications in the context for
  eventual resumption.
  """
  @impl true
  def halt(context) when has_agent?(context) do
    with {:ok, notifications} <- agent_get(context),
         {:ok, context} <- agent_stop(context) do
      if Enum.any?(notifications) do
        {:ok,
         Map.update(
           context,
           @notification_key,
           notifications,
           &Enum.concat(&1, notifications)
         )}
      else
        {:ok, context}
      end
    end
  end

  @doc """
  When the reactor completes successfully, publish any queued notifications.
  """
  @impl true
  def complete(result, context) when has_agent?(context) do
    with {:ok, notifications} <- agent_get(context),
         {:ok, context} <- agent_stop(context),
         [] <- __MODULE__.publish(context, notifications) do
      {:ok, result}
    else
      {:error, reason} ->
        {:error, reason}

      notifications when is_list(notifications) ->
        {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

        Logger.warning("""
          Missed #{Enum.count(notifications)} notifications in Reactor complete hook.

          This happens when your steps return notifications but they are unable to be published
          upon successful completion of the reactor.

          #{Exception.format_stacktrace(stacktrace)}
        """)

        {:ok, result}
    end
  end

  def complete(result, _context), do: {:ok, result}

  @doc """
  When the reactor fails, discard any queued notifications.
  """
  @impl true
  def error(_errors, context) do
    agent_stop(context)

    :ok
  end

  @doc """
  Add notifications to the queue to be published on reactor success.
  """
  @spec enqueue_notifications(Reactor.context(), Enumerable.t(Ash.Notifier.Notification.t())) ::
          :ok | {:error, any}
  def enqueue_notifications(context, notifications) do
    with {:ok, _} <- agent_put(context, notifications) do
      :ok
    end
  end

  @doc """
  Dispatch notifications.
  """
  @spec publish(
          Reactor.context(),
          Ash.Notifier.Notification.t() | [Ash.Notifier.Notification.t()]
        ) ::
          [Ash.Notifier.Notification.t()]
  def publish(context, notifications) when has_agent?(context) do
    case agent_put(context, notifications) do
      {:ok, _} -> []
      {:error, _} -> notifications
    end
  end

  def publish(_, notifications), do: Ash.Notifier.notify(notifications)

  defp agent_start(context) do
    case Agent.start_link(fn -> [] end) do
      {:ok, pid} -> {:ok, Map.update(context, @agent_key, [pid], &[pid | &1])}
      {:error, reason} -> {:error, reason}
    end
  end

  defp agent_get(%{@agent_key => [pid | _]}) do
    notifications = Agent.get(pid, & &1, 100)
    {:ok, notifications}
  rescue
    error -> {:error, error}
  end

  defp agent_get(%{@agent_key => []}),
    do: {:error, "Context does not contain a notification agent"}

  defp agent_stop(%{@agent_key => [pid | agents]} = context) do
    :ok = Agent.stop(pid, :normal)

    {:ok, %{context | @agent_key => agents}}
  rescue
    error -> {:error, error}
  end

  defp agent_put(%{@agent_key => [pid | _]} = context, notifications) do
    :ok = Agent.update(pid, &Enum.concat(&1, notifications))

    {:ok, context}
  rescue
    error -> {:error, error}
  end
end
