defmodule Ash.Engine.RequestHandler do
  defstruct [:request, :verbose?, :engine_pid, :pid_info, :runner_pid, dependencies_requested: []]

  use GenServer
  require Logger

  # TODO: as an optimization, maybe make the authorizer_state global
  # to all request_handlers (using an agent or something)

  alias Ash.Engine.Request

  ## If not bypass strict check, then the engine needs to ensure
  # that a scenario is reality *at strict check time*
  # next step after strict checking

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
        {:stop, {:error, error, %{new_request | state: :error}}, state}

      {:already_complete, new_request, notifications} ->
        new_state = %{state | request: new_request}

        notify(new_state, notifications)

        {:noreply, new_state}

      {:complete, new_request, notifications} ->
        new_state = %{state | request: new_request}
        notify(new_state, notifications)
        complete(new_state)
        {:noreply, new_state}

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
        {:stop, :shutdown, request}
    end
  end

  def handle_cast({:send_field, receiver_path, _pid, dep, optional?}, state) do
    field = List.last(dep)

    case Request.send_field(
           state.request,
           receiver_path,
           field,
           optional?
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
        {:stop, {:error, error}, %{new_request | state: :error}}
    end
  end

  def handle_cast({:field_value, _receiver_path, request_path, field, value}, state) do
    case Request.receive_field(state.request, request_path, field, value) do
      {:continue, new_request} ->
        {:noreply, %{state | request: new_request}, {:continue, :next}}
    end
  end

  defp notify(state, notifications) do
    Enum.each(notifications, fn {receiver_path, _request_path, field, value} ->
      destination_pid = Map.get(state.pid_info, receiver_path) || state.runner_pid

      log(state, "notifying #{inspect(receiver_path)} of #{inspect(field)}")

      GenServer.cast(destination_pid, {:field_value, receiver_path, field, value})
    end)
  end

  def register_dependency(state, {dep, optional?}) do
    path = :lists.droplast(dep)

    destination_pid = Map.get(state.pid_info, path) || state.runner_pid

    log(state, "registering dependency: #{inspect(dep)}")

    if not optional? and destination_pid != state.runner_pid do
      Process.link(destination_pid)
    end

    field = List.last(dep)

    log(state, "Asking #{inspect(path)} for #{field}")

    GenServer.cast(
      destination_pid,
      {:send_field, state.request.path, self(), dep, optional?}
    )

    unless optional? do
      log(state, "Registering hard dependency on #{inspect(path)} - #{field}")

      GenServer.cast(
        state.engine_pid,
        {:register_dependency, state.request.path, self(), dep}
      )
    end

    :ok
  end

  def send_field_value(pid, receiver_path, request_path, field, value) do
    GenServer.cast(pid, {:field_value, receiver_path, request_path, field, value})
  end

  defp complete(state) do
    log(state, "Request complete, sending data")
    GenServer.cast(state.engine_pid, {:complete, state.request.path})
    send(state.runner_pid, {:data, state.request.path, state.request.data})
  end

  defp log(state, message, level \\ :debug)

  defp log(%{verbose?: true, request: request}, message, level) do
    Logger.log(level, "#{request.name}: #{message}")
  end

  defp log(_, _, _) do
    false
  end
end
