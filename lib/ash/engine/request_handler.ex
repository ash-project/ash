defmodule Ash.Engine.RequestHandler do
  @moduledoc false
  defstruct [:request, :verbose?, :engine_pid, :pid_info, :runner_pid, dependencies_requested: []]

  use GenServer
  require Logger

  alias Ash.Engine.Request

  def init(opts) do
    state = %__MODULE__{
      request: opts[:request],
      verbose?: opts[:verbose?] || false,
      runner_pid: opts[:runner_pid],
      engine_pid: opts[:engine_pid]
    }

    log(state, "Starting request")

    {:ok, state, {:continue, :next}}
  end

  def handle_continue(:next, %{pid_info: nil} = state) do
    receive do
      {:pid_info, pid_info} ->
        {:noreply, %{state | pid_info: pid_info || %{}}, {:continue, :next}}
    end
  end

  def handle_continue(:next, %{request: request} = state) do
    case Request.next(request) do
      {:continue, new_request, notifications} ->
        new_state = %{state | request: new_request}
        notify(new_state, notifications)

        {:noreply, new_state, {:continue, :next}}

      {:error, error, new_request} ->
        new_request = %{new_request | state: :error}
        new_state = %{state | request: new_request}
        notify_error(new_state, error)

        {:noreply, new_state}

      {:already_complete, new_request, notifications, dependencies} ->
        new_state = %{state | request: new_request}
        Enum.each(dependencies, &register_dependency(new_state, &1))

        notify(new_state, notifications)

        {:noreply, new_state}

      {:complete, new_request, notifications, dependencies} ->
        new_state = %{state | request: new_request}
        notify(new_state, notifications)
        Enum.each(dependencies, &register_dependency(new_state, &1))

        if new_request.notify? do
          resource_notification = Request.resource_notification(new_request)
          send(new_state.runner_pid, {:runner, :notification, resource_notification})
        end

        complete(new_state)

        {:noreply, new_state, {:continue, :next}}

      {:wait, new_request, notifications, dependencies} ->
        new_state = %{state | request: new_request}
        notify(new_state, notifications)
        Enum.each(dependencies, &register_dependency(new_state, &1))

        {:noreply, new_state}
    end
  end

  def handle_cast({:wont_receive, _receiver_path, path, field}, state) do
    case Request.wont_receive(state.request, path, field) do
      {:stop, :dependency_failed, request} ->
        new_state = %{state | request: %{request | state: :error}}
        notify_error(new_state, :dependency_failed)
        {:noreply, new_state}
    end
  end

  def handle_cast(
        {:send_field, receiver_path, pid, dep},
        %{request: %{path: path, state: :error}} = state
      ) do
    if pid == state.runner_pid do
      send(pid, {:wont_receive, receiver_path, path, List.last(dep)})
    else
      GenServer.cast(
        pid,
        {:wont_receive, receiver_path, path, List.last(dep)}
      )
    end

    {:noreply, state}
  end

  def handle_cast({:send_field, receiver_path, _pid, dep}, state) do
    field = List.last(dep)

    case Request.send_field(
           state.request,
           receiver_path,
           field
         ) do
      {:waiting, new_request, notifications, dependency_requests} ->
        new_state = %{state | request: new_request}
        notify(new_state, notifications)
        Enum.each(dependency_requests, &register_dependency(new_state, &1))

        {:noreply, new_state}

      {:ok, new_request, notifications} ->
        new_state = %{state | request: new_request}

        notify(new_state, notifications)
        {:noreply, new_state}

      {:error, error, new_request} ->
        new_request = %{new_request | state: :error}
        new_state = %{state | request: new_request}
        notify_error(new_state, error)
        {:noreply, new_state}
    end
  end

  def handle_cast({:field_value, _receiver_path, request_path, field, value}, state) do
    case Request.receive_field(state.request, request_path, field, value) do
      {:continue, new_request} ->
        {:noreply, %{state | request: new_request}, {:continue, :next}}
    end
  end

  defp notify_error(state, error) do
    log(state, "Request error, notifying engine")
    GenServer.cast(state.engine_pid, {:error, error, state})
  end

  defp notify(state, notifications) do
    Enum.each(notifications, fn
      {:set_extra_data, key, value} ->
        send(state.runner_pid, {:data, [key], value})

      %Ash.Notifier.Notification{} = resource_notification ->
        send(state.runner_pid, {:runner, :notification, resource_notification})

      {receiver_path, request_path, field, value} ->
        receiver_path =
          case receiver_path do
            {_, path} -> path
            path -> path
          end

        destination_pid = Map.get(state.pid_info, receiver_path) || state.runner_pid

        log(state, "notifying #{inspect(receiver_path)} of #{inspect(field)}")

        if destination_pid == state.runner_pid do
          send(destination_pid, {:field_value, receiver_path, request_path, field, value})
        else
          GenServer.cast(
            destination_pid,
            {:field_value, receiver_path, request_path, field, value}
          )
        end
    end)
  end

  def register_dependency(state, dep) do
    path = :lists.droplast(dep)

    destination_pid = Map.get(state.pid_info, path) || state.runner_pid

    log(state, "registering dependency: #{inspect(dep)}")

    field = List.last(dep)

    log(state, "Asking #{inspect(path)} for #{field}")

    if destination_pid == state.runner_pid do
      send(
        destination_pid,
        {:send_field, state.request.path, self(), dep}
      )
    else
      GenServer.cast(
        destination_pid,
        {:send_field, state.request.path, self(), dep}
      )
    end

    log(state, "Registering dependency on #{inspect(path)} - #{field}")

    GenServer.cast(
      state.engine_pid,
      {:register_dependency, state.request.path, self(), dep}
    )

    :ok
  end

  defp complete(state) do
    log(state, "Request complete, sending data")
    send(state.runner_pid, {:data, state.request.path, state.request.data})
    GenServer.cast(state.engine_pid, {:complete, state.request.path})
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, request: request}, message, level) do
    Logger.log(level, "#{request.name}: #{message}")
  end

  defp log(_, _, _) do
    false
  end
end
