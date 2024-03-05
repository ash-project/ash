defmodule Ash.Reactor.Notifications do
  @moduledoc """
  Reactor middleware used to collect and emit notifications upon successful
  completion of the Reactor.
  """

  use Reactor.Middleware

  require Logger

  @context_agent_key :__ash_notification_agent__
  @context_notification_key :__unpublished_ash_notifications__

  defguardp has_agent?(context) when is_map_key(context, @context_agent_key)

  defguardp has_notifications?(context)
            when is_map_key(context, @context_notification_key) and
                   length(:erlang.map_get(@context_notification_key, context)) > 0

  @doc """
  When starting a reactor, start an agent to act as a temporary store of
  notifications.
  """
  @impl true
  def init(context) when has_notifications?(context) do
    with {:ok, notifications} <- Map.fetch(context, @context_notification_key),
         {:ok, context} <- agent_start(context),
         {:ok, context} <- agent_put(context, notifications) do
      context = Map.delete(context, @context_notification_key)
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
           @context_notification_key,
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
         {:ok, _context} <- agent_stop(context),
         [] <- __MODULE__.publish(notifications) do
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
  @spec publish(Ash.Notifier.Notification.t() | [Ash.Notifier.Notification.t()]) ::
          [Ash.Notifier.Notification.t()]
  def publish(notifications), do: Ash.Notifier.notify(notifications)

  defp agent_start(context) when has_agent?(context) do
    case agent_get(context) do
      {:ok, _} -> {:ok, context}
      _ -> agent_start(Map.delete(context, @context_agent_key))
    end
  end

  defp agent_start(context) do
    case Agent.start_link(fn -> [] end) do
      {:ok, pid} -> {:ok, Map.put(context, :__ash_notification_agent__, pid)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp agent_get(context) do
    notifications =
      context
      |> Map.fetch!(@context_agent_key)
      |> Agent.get(fn notifications -> notifications end, 100)

    {:ok, notifications}
  rescue
    error -> {:error, error}
  end

  defp agent_stop(context) do
    :ok =
      context
      |> Map.fetch!(@context_agent_key)
      |> Agent.stop(:normal)

    {:ok, Map.delete(context, @context_agent_key)}
  rescue
    error -> {:error, error}
  end

  defp agent_put(context, notifications) do
    :ok =
      context
      |> Map.fetch!(@context_agent_key)
      |> Agent.update(&Enum.concat(&1, notifications))

    {:ok, context}
  rescue
    error -> {:error, error}
  end
end
