defmodule Ash.Engine.RequestHandler do
  @moduledoc false
  use GenServer

  defstruct [
    :request,
    :verbose?,
    :engine_pid,
    :pid_info,
    :runner_pid,
    :runner_ref,
    dependencies_requested: [],
    notifications_sent: MapSet.new()
  ]

  alias Ash.Engine.Request

  require Logger

  def init(opts) do
    Process.put(:"$callers", opts[:callers])

    state = %__MODULE__{
      request: opts[:request],
      verbose?: opts[:verbose?] || false,
      runner_pid: opts[:runner_pid],
      engine_pid: opts[:engine_pid],
      runner_ref: opts[:runner_ref]
    }

    if opts[:engine_pid] do
      Process.monitor(opts[:engine_pid])
    end

    if opts[:runner_pid] do
      Process.monitor(opts[:runner_pid])
    end

    log(state, fn -> "Starting request" end)

    {:ok, state, {:continue, :next}}
  end

  def handle_continue(:next, %{pid_info: nil} = state) do
    receive do
      {:pid_info, pid_info} ->
        {:noreply, %{state | pid_info: pid_info || %{}}, {:continue, :next}}
    end
  end

  def handle_continue(:next, %{request: %{state: :error}} = state) do
    {:noreply, state}
  end

  def handle_continue(:next, %{request: request} = state) do
    case Request.next(request) do
      {:continue, new_request, notifications} ->
        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        {:noreply, new_state, {:continue, :next}}

      {:error, error, notifications, new_request} ->
        new_request = %{new_request | state: :error}

        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        notify_error(new_state, error)

        {:noreply, new_state}

      {:error, error, new_request} ->
        new_request = %{new_request | state: :error}
        new_state = %{state | request: new_request}
        notify_error(new_state, error)

        {:noreply, new_state}

      {:already_complete, new_request, notifications, dependencies} ->
        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        Enum.each(dependencies, &register_dependency(new_state, &1))

        {:noreply, new_state}

      {:complete, new_request, notifications, dependencies} ->
        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        Enum.each(dependencies, &register_dependency(new_state, &1))

        if new_request.notify? do
          resource_notification = Request.resource_notification(new_request)

          send(
            new_state.runner_pid,
            {state.runner_ref, {:notification, resource_notification}}
          )
        end

        complete(new_state)

        {:noreply, new_state, {:continue, :next}}

      {:wait, new_request, notifications, dependencies} ->
        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        Enum.each(dependencies, &register_dependency(new_state, &1))

        {:noreply, new_state}
    end
  end

  def handle_info({:pid_info, pid_info}, state) do
    {:noreply, %{state | pid_info: pid_info}}
  end

  def handle_info({:DOWN, _, _, pid, reason}, state) do
    reason =
      case reason do
        {:shutdown, _} -> :shutdown
        reason -> reason
      end

    if pid == state.runner_pid do
      log(state, fn -> "Runner down #{state.request.name}: #{inspect(reason)}" end)
    else
      log(state, fn -> "Engine down #{state.request.name}: #{inspect(reason)}" end)
    end

    {:stop, reason, state}
  end

  def handle_call(
        {:send_field, receiver_path, pid, dep},
        _,
        %{request: %{path: path, state: :error}} = state
      ) do
    if pid == state.runner_pid do
      send(pid, {state.runner_ref, {:wont_receive, receiver_path, path, List.last(dep)}})
    else
      GenServer.cast(
        pid,
        {:wont_receive, receiver_path, path, List.last(dep)}
      )
    end

    {:reply, :ok, state}
  end

  def handle_call({:send_field, receiver_path, _pid, dep}, _, state) do
    field = List.last(dep)

    case Request.send_field(
           state.request,
           receiver_path,
           field
         ) do
      {:waiting, new_request, notifications, dependency_requests} ->
        new_state =
          state
          |> Map.put(:request, new_request)
          |> notify(notifications)

        Enum.each(dependency_requests, &register_dependency(new_state, &1))

        {:reply, :ok, new_state}

      {:ok, new_request, notifications} ->
        new_state =
          %{state | request: new_request}
          |> notify(notifications)

        {:reply, :ok, new_state}

      {:error, error, new_request} ->
        new_request = %{new_request | state: :error}
        new_state = %{state | request: new_request}
        notify_error(new_state, error)
        {:reply, :ok, new_state}
    end
  end

  def handle_cast({:wont_receive, _receiver_path, path, field}, state) do
    case Request.wont_receive(state.request, path, field) do
      {:stop, :dependency_failed, request} ->
        new_state = %{state | request: %{request | state: :error}}
        notify_error(new_state, {:dependency_failed, path})
        {:noreply, new_state}
    end
  end

  def handle_cast({:field_value, _receiver_path, request_path, field, value}, state) do
    case Request.receive_field(state.request, request_path, field, value) do
      {:continue, new_request} ->
        {:noreply, %{state | request: new_request}, {:continue, :next}}
    end
  end

  def handle_cast(:log_stuck_report, state) do
    log(
      state,
      fn ->
        "Synchronous runner stuck: async request handler current state #{
          inspect(state.request.path)
        } #{state.request.state}"
      end
    )

    {:noreply, state}
  end

  defp notify_error(state, error) do
    log(state, fn -> "Request error, notifying engine" end)
    GenServer.cast(state.engine_pid, {:error, error, state})
  end

  defp notify(state, notifications) do
    Enum.reduce(notifications, state, fn
      {:set_extra_data, key, value}, state ->
        send(state.runner_pid, {state.runner_ref, {:data, [key], value}})
        state

      {:requests, new_requests}, state ->
        unless Enum.empty?(new_requests) do
          GenServer.cast(state.engine_pid, {:new_requests, new_requests})
        end

        state

      {:update_changeset, changeset}, state ->
        send(state.runner_pid, {state.runner_ref, {:update_changeset, changeset}})
        state

      %Ash.Notifier.Notification{} = resource_notification, state ->
        send(
          state.runner_pid,
          {state.runner_ref, {state.runner_ref, {:notification, resource_notification}}}
        )

        state

      {receiver_path, request_path, field, value}, state ->
        if MapSet.member?(state.notifications_sent, {receiver_path, request_path, field}) do
          state
        else
          receiver_path =
            case receiver_path do
              {_, path} -> path
              path -> path
            end

          destination_pid = Map.get(state.pid_info, receiver_path) || state.runner_pid

          log(state, fn -> "notifying #{inspect(receiver_path)} of #{inspect(field)}" end)

          if destination_pid == state.runner_pid do
            send(
              destination_pid,
              {state.runner_ref, {:field_value, receiver_path, request_path, field, value}}
            )
          else
            GenServer.cast(
              destination_pid,
              {:field_value, receiver_path, request_path, field, value}
            )
          end

          Map.update!(
            state,
            :notifications_sent,
            &MapSet.put(&1, {receiver_path, request_path, field})
          )
        end
    end)
  end

  def register_dependency(state, dep) do
    path = :lists.droplast(dep)

    destination_pid = Map.get(state.pid_info, path) || state.runner_pid

    log(state, fn -> "registering dependency: #{inspect(dep)}" end)

    field = List.last(dep)

    log(state, fn -> "Asking #{inspect(path)} for #{field}" end)

    if destination_pid == state.runner_pid do
      send(
        destination_pid,
        {state.runner_ref, {:send_field, state.request.path, self(), dep}}
      )
    else
      GenServer.call(
        destination_pid,
        {:send_field, state.request.path, self(), dep},
        :infinity
      )
    end

    log(state, fn -> "Registering dependency on #{inspect(path)} - #{field}" end)

    GenServer.cast(
      state.engine_pid,
      {:register_dependency, state.request.path, self(), dep}
    )

    :ok
  end

  defp complete(state) do
    log(state, fn -> "Request complete, sending data" end)
    send(state.runner_pid, {state.runner_ref, {:data, state.request.path, state.request.data}})
    GenServer.cast(state.engine_pid, {:complete, state.request.path})
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, request: request}, message, level) do
    Logger.log(level, fn -> [request.name, message.()] end)
  end

  defp log(_, _, _) do
    false
  end
end
